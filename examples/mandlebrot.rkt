#lang racket/base

(require "../simple.rkt")
(require "../private/simple/base.rkt")
(require "../private/short.rkt")
(require ffi/unsafe)
(require racket/flonum)
(require racket/cmdline)
(require racket/fixnum)
(require 
  (only-in "../safe.rkt"
    LLVMBuildFMul
    LLVMOptimizeModule
    LLVMGetTarget
    LLVMSetTarget
    LLVMSetVisibility 
    LLVMSetLinkage 
    LLVMCreateExecutionEngineForModule
    LLVMCreateJITCompilerForModule
    LLVMCreateGenericValueOfInt
    LLVMCreateGenericValueOfFloat
    LLVMGenericValueToFloat
    LLVMCreateGenericValueOfPointer
    LLVMRunFunction))


(define context (llvm-create-context))
(current-context context)
(define module (llvm-create-module "mandelbrot"))
(current-module module)
(current-integer-type (llvm-int32-type))
(current-float-type (llvm-double-type))
(current-builder (llvm-create-builder))

(define int-type (llvm-int-type))
(define bool-type (llvm-int1-type))
(define float-type (current-float-type))


(define mandel-size 8)
(define (mandel-vec type) (ll (vec type mandel-size)))


(define calculate-row-type
  (ll (-> float (ptr (arr float)) (ptr (arr i8)) void)))


(define mandelbrot-type
  (ll (-> (mandel-vec float)
          (mandel-vec float)
          (mandel-vec bool))))




(define LIMIT-SQR 4.0) 
(define ITERATIONS 50) 
(define N (command-line #:args (n) (string->number n))) 
(define N.0 (fx->fl N)) 
(define 2/N (fl/ 2.0 N.0)) 


(define calculate-row (llvm-add-function calculate-row-type "calculate_row"))
(define mandelbrot (llvm-add-function mandelbrot-type "mandelbrot"))
(LLVMSetVisibility mandelbrot 'LLVMHiddenVisibility)
(LLVMSetLinkage mandelbrot 'LLVMPrivateLinkage)

(define (llvm-and* vector)
  (for/fold ((acc (llvm-int 1 (llvm-int1-type))))
            ((i (llvm-get-vector-type-size (value->llvm-type vector))))
    (llvm-and acc (llvm-extract-element vector i))))

(define (mandel-vector v)
  (llvm-vector* (build-list mandel-size (lambda (i) v))))

(void (ll (let ()
  (define entry-block (llvm-add-block-to-function mandelbrot #:name "entry")) 
  (llvm-set-position entry-block)
  (define cr (llvm-get-param 0))
  (define ci (llvm-get-param 1))
  (define LIMIT (mandel-vector 4.0))

  (llvm-define-mutable zr (mandel-vector 0.0))
  (llvm-define-mutable zi (mandel-vector 0.0))

  (define (compute-magnitude)
    (+ (* zr zr) (* zi zi)))

  (for i 0 (< i ITERATIONS) (+ i 1)
    (when (llvm-and* (> (compute-magnitude)  LIMIT)) 
        (return (mandel-vector #f)))
    (set!-values (zr zi)
      (values
        (+ (- (* zr zr) (* zi zi)) cr)
        (+ (* (mandel-vector 2.0) (* zr zi)) ci))))

  (return (< (compute-magnitude) LIMIT)))))


(define minus -)

(void (let () (ll
  (define entry-block (llvm-add-block-to-function calculate-row #:name "entry")) 
  (llvm-set-position entry-block)

  (for i 0 (< i N) (+ i 8)
    (define bits
     (apply append
      (for/list ((j (in-range 0 8 mandel-size)))
        (let* ((ci (llvm-get-param 0))
               (crs (llvm-get-param 1))
               (crs2 (for/list ((k mandel-size))
                        (load (gep crs 0 (+ i (+ j k)))))))
           (let ((bits (call mandelbrot (llvm-vector* crs2) (mandel-vector ci))))
             (for/list ((i mandel-size))
              (llvm-extract-element bits i)))))))
    (llvm-store
     (for/fold ((ans (llvm-int 0 (llvm-int8-type))))
               ((j 8) (b bits))
        (or ans 
           (<< (zext b i8)
               (llvm-int (minus 7 j) i8))))
     (gep (llvm-get-param 2) 0 (/ i 8))))


  (llvm-ret-void))))
  

(let ((err (llvm-verify-module module)))
  (void
   (and err
        (error 'mandelbrot "Bad module: ~a, module" err))))


;(display (llvm-module-description module) (current-error-port))
(void (LLVMOptimizeModule module))

;(display (llvm-module-description module))

(define ee (LLVMCreateJITCompilerForModule module 3))

(define Crs
  (let ([v (make-flvector N)])
    (for ([x (in-range N)])
      (flvector-set! v x (fl- (fl/ (fx->fl (fx* 2 x)) N.0) 1.5)))
    v))

(define O (current-output-port))
(fprintf O "P4\n~a ~a\n" N N)
(define byte-array (make-bytes (ceiling (/ N 8))))
(define output-row (LLVMCreateGenericValueOfPointer byte-array))
(define gen-Crs (LLVMCreateGenericValueOfPointer (flvector->cpointer Crs)))

(let loop-y ([y N])
  (let ([Ci (fl- (fl* 2/N (fx->fl y)) 1.0)])
   (let ((gen-Ci (LLVMCreateGenericValueOfFloat float-type Ci)))
     (LLVMRunFunction ee calculate-row (list gen-Ci gen-Crs output-row))
     (write-bytes byte-array O)
     (when (fx> y 1) (loop-y (fx- y 1))))))
   
                                                                                                         
                                                                                                         





