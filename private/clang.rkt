#lang racket/base

(require ffi/unsafe racket/format racket/list ffi/unsafe/alloc)
(require "llvm-config.rkt")
(provide (all-defined-out))

(define lib-clang (ffi-lib (build-path (llvm-lib-dir) "libclang")))

;; Simple data types
(define _CXCursorKind  _uint)
(define-cstruct _CXCursor
  ([kind _CXCursorKind]
   [xdata _int]
   [data (_array _pointer 3)]))

(define _CXChildVisitResult 
  (_enum
    '(CXChildVisit_Break
      CXChildVisit_Continue
      CXChildVisit_Recurse)))

(define _CXClientData _racket)

(define _CXCursorVisitor
  (_fun _CXCursor _CXCursor _CXClientData -> _CXChildVisitResult))


(define (typedef-decl-kind? kind)
  (equal? kind 20))
(define (enum-decl-kind? kind)
  (equal? kind 5))
(define (macro-def-kind? kind)
  (equal? kind 501))

;; Index types
(provide create-index)
(struct clang-index ([pointer]))

(define _CXIndex
  (make-ctype _pointer
    clang-index-pointer
    (λ (v) (error '_CXIndex "Conversion not supported"))))


(define raw-create-index
  (get-ffi-obj "clang_createIndex" lib-clang (_fun _bool _bool -> _pointer))) 
(define dispose-index
  (get-ffi-obj "clang_disposeIndex" lib-clang (_fun _pointer -> _void))) 

(define protected-create-index
  ((allocator dispose-index) raw-create-index))

(define (create-index #:exclude-declarations-from-pch [exclude #f]
                      #:display-diagnostics [diagnostics #f])
  (clang-index (protected-create-index exclude diagnostics)))


;; String functions
(define clang-get-cstring
  (get-ffi-obj "clang_getCString" lib-clang
    (_fun _pointer -> _string)))
(define clang-dispose-string
  (get-ffi-obj "clang_disposeString" lib-clang
    (_fun _pointer -> _string)))

(define _CXString
  (make-ctype _pointer
    (λ (v) (error '_CXString "Conversion not supported"))
    (λ (p)
       (begin0
         (clang-get-cstring p)
         (clang-dispose-string p)))))

;; Translation unit types
(struct clang-translation-unit ([pointer]))
(define _CXTranslationUnit
  (make-ctype _pointer
    clang-translation-unit-pointer
    (λ (v) (error '_CXTranslationUnit "Conversion not supported"))))


(define dispose-translation-unit
  (get-ffi-obj "clang_disposeTranslationUnit" lib-clang (_fun _pointer -> _void))) 

(define raw-create-translation-unit-from-source-file
  (get-ffi-obj "clang_createTranslationUnitFromSourceFile" lib-clang
    (_fun (index source-file-name flags unsaved-files) ::
      (index : _CXIndex) (source-file-name : _string)
      (_int = (length flags)) (flags : (_list i _string))
      (_int = (length unsaved-files)) (unsaved-files : (_list i _pointer))
      -> _pointer)))

(define protected-create-translation-unit-from-source-file
  ((allocator dispose-translation-unit) raw-create-translation-unit-from-source-file))

(define (create-translation-unit-from-source-file index source-file [flags '()] [unsaved-files '()])
  (clang-translation-unit
    (protected-create-translation-unit-from-source-file index source-file flags unsaved-files)))

;;;

(define translation-unit-cursor
  (get-ffi-obj "clang_getTranslationUnitCursor" lib-clang
    (_fun _CXTranslationUnit -> _CXCursor)))

(define cursor-visit-children
  (get-ffi-obj "clang_visitChildren" lib-clang
    (_fun _CXCursor _CXCursorVisitor _CXClientData -> _bool)))

(define enum-constant-decl-value
  (get-ffi-obj "clang_getEnumConstantDeclValue" lib-clang
    (_fun _CXCursor -> _uint)))

(define cursor-spelling
  (get-ffi-obj "clang_getCursorSpelling" lib-clang
    (_fun _CXCursor -> _CXString)))

;; Folds over cursor-visit-children
(define (cursor-fold cursor f init)
  (define acc (box init))
  (cursor-visit-children cursor
    (λ (child parent data)
       (set-box! data (f child (unbox data)))
       'CXChildVisit_Continue)
    acc)
  (unbox acc))

(define (cursor-map cursor f)
  (reverse
    (cursor-fold cursor
      (λ (c acc) (cons (f c) acc))
      null)))

(define (cursor-find cursor f)
  (define acc (box #f))
  (and
    (cursor-visit-children cursor
      (λ (child parent data)
         (define v (f child))
         (cond
           [v
            (set-box! data v)
            'CXChildVisit_Break]
           [else 'CXChildVisit_Continue]))
      acc)
    (unbox acc)))

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
