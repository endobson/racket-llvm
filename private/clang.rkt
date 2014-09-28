#lang racket/base

(require ffi/unsafe racket/format racket/list ffi/unsafe/alloc)
(require "llvm-config.rkt")
(provide
  create-index
  index?
  create-translation-unit-from-source-file
  translation-unit?
  translation-unit-cursor
  cursor?
  cursor-map
  cursor-spelling
  cursor-find

  enum-constant-decl-value
  namespace-decl-cursor?
  enum-decl-cursor?
  typedef-decl-cursor?)

(define lib-clang (ffi-lib (build-path (llvm-lib-dir) "libclang")))

;; Indices
(struct index (pointer))

(define _index
  (make-ctype _pointer
    index-pointer
    (λ (v) (error '_index "Conversion not supported"))))

(define create-index
  (let ()
    (define dispose-index
      (get-ffi-obj "clang_disposeIndex" lib-clang (_fun _pointer -> _void))) 
    (define create-index
      ((allocator dispose-index)
       (get-ffi-obj "clang_createIndex" lib-clang (_fun _bool _bool -> _pointer))) )
    
    (lambda (#:exclude-declarations-from-pch [exclude #f] #:display-diagnostics [diagnostics #f])
      (index (create-index exclude diagnostics)))))

;; String functions
(define _CXString
  (let ()
    (define-cstruct _CXString
      ([data _pointer]
       [flags _uint]))
  
  (define get-cstring
    (get-ffi-obj "clang_getCString" lib-clang
      (_fun _CXString -> _string)))
  
  (define dispose-string
    (get-ffi-obj "clang_disposeString" lib-clang
      (_fun _CXString -> _void)))
  (make-ctype _CXString
    (λ (v) (error '_CXString "Conversion not supported"))
    (λ (v)
       (begin0
         (get-cstring v)
         (dispose-string v))))))

;; Translation units
(struct translation-unit (owner pointer))
(define _translation-unit
  (make-ctype _pointer
    translation-unit-pointer
    (λ (v) (error '_CXTranslationUnit "Conversion not supported"))))

(define create-translation-unit-from-source-file
  (let ()
    (define dispose-translation-unit
      (get-ffi-obj "clang_disposeTranslationUnit" lib-clang (_fun _pointer -> _void))) 
    (define create-translation-unit-from-source-file
      ((allocator dispose-translation-unit) 
       (get-ffi-obj "clang_createTranslationUnitFromSourceFile" lib-clang
         (_fun (index source-file-name flags unsaved-files) ::
           (index : _index) (source-file-name : _string)
           (_int = (length flags)) (flags : (_list i _string))
           (_int = (length unsaved-files)) (unsaved-files : (_list i _pointer))
           -> _pointer))))

    (lambda (index source-file [flags '()] [unsaved-files '()])
      (translation-unit
        index
        (create-translation-unit-from-source-file index source-file flags unsaved-files)))))


;; Cursors
(define _CXCursorKind  _uint)
(define-cstruct _CXCursor
  ([kind _CXCursorKind]
   [xdata _int]
   [data (_array _pointer 3)]))

(struct cursor (owner struct))
(define _cursor
  (make-ctype _CXCursor
    cursor-struct
    (λ (v) (error '_cursor "Conversion not supported"))))

;; Cursor constructor
(define translation-unit-cursor
  (get-ffi-obj "clang_getTranslationUnitCursor" lib-clang
    (_fun (tu : _translation-unit)
          -> (c : _CXCursor)
          -> (cursor tu c))))

;; Cursor traversal
(define _CXChildVisitResult 
  (_enum
    '(CXChildVisit_Break
      CXChildVisit_Continue
      CXChildVisit_Recurse)))
(define _CXClientData _pointer)
(define _CXCursorVisitor
  (_fun _CXCursor _CXCursor _CXClientData -> _CXChildVisitResult))


(define cursor-visit-children
  (get-ffi-obj "clang_visitChildren" lib-clang
    (_fun _cursor _CXCursorVisitor _CXClientData -> _bool)))

;; Cursor Accessors
(define cursor-spelling
  (get-ffi-obj "clang_getCursorSpelling" lib-clang
    (_fun _cursor -> _CXString)))
(define enum-constant-decl-value
  (get-ffi-obj "clang_getEnumConstantDeclValue" lib-clang
    (_fun _cursor -> _uint)))

;; Cursor helpers
(define (enum-decl-cursor? cursor)
  (equal? (CXCursor-kind (cursor-struct cursor)) 5))
(define (typedef-decl-cursor? cursor)
  (equal? (CXCursor-kind (cursor-struct cursor)) 20))
(define (namespace-decl-cursor? cursor)
  (equal? (CXCursor-kind (cursor-struct cursor)) 22))



;; Folds over cursor-visit-children
(define (cursor-fold c f init)
  (define acc (malloc-immobile-cell init))
  (cursor-visit-children c
    (λ (child parent data)
       (define new-child (cursor (cursor-owner c) child))
       
       (ptr-set! data _racket (f new-child (ptr-ref data _racket)))
       'CXChildVisit_Continue)
    acc)
  (begin0
    (ptr-ref acc _racket)
    (free-immobile-cell acc)))

(define (cursor-map c f)
  (reverse
    (cursor-fold c
      (λ (c acc) (cons (f c) acc))
      null)))

(define (cursor-find c f)
  (define acc (malloc-immobile-cell #f))
  (and
    (cursor-visit-children c
      (λ (child parent data)
        (define new-child (cursor (cursor-owner c) child))
        (define v (f new-child))
        (cond
          [v
           (ptr-set! data _racket v)
           'CXChildVisit_Break]
          [else 'CXChildVisit_Continue]))
      acc)
    (begin0
      (ptr-ref acc _racket)
      (free-immobile-cell acc))))

#|
(define idx (create-index))

;(define tu (create-translation-unit-from-source-file idx "/Users/endobson/tmp/clang/tmp.c"))
(define tu (create-translation-unit-from-source-file idx
             (string-append (llvm-include-dir) "/clang-c/Index.h")))

(define cursor (translation-unit-cursor tu))

(define (enum-decl->_enum cursor)
  (_enum
    (cursor-map cursor
      (λ (child)
        (list (cursor-spelling child) (enum-constant-decl-value child))))))

(filter values
  (cursor-map cursor
    (λ (cursor)
       (cond
         [(typedef-decl-kind? (CXCursor-kind cursor))
          (define enum (cursor-find cursor enum-decl->_enum))
          (and enum (list 'typedef (cursor-spelling cursor) enum))]
         [(enum-decl-kind? (CXCursor-kind cursor))
          (define name (cursor-spelling cursor))
          (and (not (equal? "" name))
               (list 'enum name (enum-decl->_enum cursor)))]
         [else #f]))))

|#
