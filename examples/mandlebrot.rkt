#lang racket/base

(require "../simple.rkt")
(require "../private/simple/base.rkt")
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

(define crs-type (llvm-array-type float-type))
(define output-row-type (llvm-array-type (llvm-int8-type)))
(define calculate-row-type
  (llvm-function-type
    (llvm-void-type)
    float-type
    (llvm-pointer-type crs-type)
    (llvm-pointer-type output-row-type)))


(define mandelbrot-type
  (llvm-function-type
    bool-type
    float-type
    float-type))



(define mandel-size 1)

(define mandelbrot2-type
  (llvm-function-type
    (llvm-vector-type bool-type mandel-size)
    (llvm-vector-type float-type mandel-size)
    (llvm-vector-type float-type mandel-size)))




(define LIMIT-SQR 4.0) 
(define ITERATIONS 50) 
(define N (command-line #:args (n) (string->number n))) 
(define N.0 (fx->fl N)) 
(define 2/N (fl/ 2.0 N.0)) 


(define calculate-row (llvm-add-function calculate-row-type "calculate_row"))
(define mandelbrot (llvm-add-function mandelbrot-type "mandelbrot"))
(define mandelbrot2 (llvm-add-function mandelbrot2-type "mandelbrot2"))
(LLVMSetVisibility mandelbrot 'LLVMHiddenVisibility)
(LLVMSetLinkage mandelbrot 'LLVMPrivateLinkage)

(define (llvm-and* vector size)
  (for/fold ((acc (llvm-int 1 (llvm-int1-type)))) ((i size))
    (llvm-and acc (llvm-extract-element vector i))))

(define (llvm-vector* v size)
  (apply llvm-vector (build-list size (lambda (i) v))))

(let ()
  (define entry-block (llvm-add-block-to-function mandelbrot2 #:name "entry")) 
  (llvm-set-position entry-block)
  (define cr (llvm-get-param 0))
  (define ci (llvm-get-param 1))
  (define LIMIT (llvm-vector* 4.0 mandel-size))

  (define zr-box (llvm-alloca (llvm-vector-type float-type mandel-size)))
  (define zi-box (llvm-alloca (llvm-vector-type float-type mandel-size)))
  (llvm-store (llvm-vector* 0.0 mandel-size) zr-box)
  (llvm-store (llvm-vector* 0.0 mandel-size) zi-box)

  (llvm-for i 0 (llvm-icmp '< i ITERATIONS) (llvm+ i 1)
    (define zr (llvm-load zr-box))
    (define zi (llvm-load zi-box))
    (define magnitude (llvm-fl+ (llvm-fl* zr zr) (llvm-fl* zi zi))) 
    (define dual-bits (llvm-fcmp '> magnitude LIMIT))
    (llvm-when (llvm-and* dual-bits mandel-size) 
        (llvm-ret (llvm-vector* #f mandel-size)))
    (define zr-next (llvm-fl+ (llvm-fl- (llvm-fl* zr zr) (llvm-fl* zi zi)) cr))
    (define zi-next (llvm-fl+ (llvm-fl* (llvm-vector* 2.0 mandel-size) (llvm-fl* zr zi)) ci))
    (llvm-store zr-next zr-box)
    (llvm-store zi-next zi-box))

  (let ()
    (define zr (llvm-load zr-box))
    (define zi (llvm-load zi-box))
    (define magnitude (llvm-fl+ (llvm-fl* zr zr) (llvm-fl* zi zi))) 
    (llvm-ret (llvm-fcmp '< magnitude LIMIT)))

  (void))


(let ()
  (define entry-block (llvm-add-block-to-function mandelbrot #:name "entry")) 
  (llvm-set-position entry-block)
  (define cr (llvm-get-param 0))
  (define ci (llvm-get-param 1))
  (define LIMIT 4.0)

  (define zr-box (llvm-alloca float-type))
  (define zi-box (llvm-alloca float-type))
  (llvm-store 0.0 zr-box)
  (llvm-store 0.0 zi-box)

  (llvm-for i 0 (llvm-icmp '<= i ITERATIONS) (llvm+ i 1)
    (define zr (llvm-load zr-box))
    (define zi (llvm-load zi-box))
    (define magnitude (llvm-fl+ (llvm-fl* zr zr) (llvm-fl* zi zi))) 
    (llvm-when (llvm-fcmp '> magnitude LIMIT)
        (llvm-ret #f))
    (define zr-next (llvm-fl+ (llvm-fl- (llvm-fl* zr zr) (llvm-fl* zi zi)) cr))
    (define zi-next (llvm-fl+ (llvm-fl* 2.0 (llvm-fl* zr zi)) ci))
    (llvm-store zr-next zr-box)
    (llvm-store zi-next zi-box))
  (llvm-ret #t)

  (void))


(let ()
  (define entry-block (llvm-add-block-to-function calculate-row #:name "entry")) 
  (llvm-set-position entry-block)


  (llvm-for i 0 (llvm-< i N) (llvm+ i 8)
    (define bits
     (apply append
      (for/list ((j (in-range 0 8 mandel-size)))
        (let* ((ci (llvm-get-param 0))
               (crs (llvm-get-param 1))
               (crs2 (for/list ((j mandel-size))
                        (llvm-load (llvm-gep crs 0 (llvm+ i j))))))
           (let ((bits (llvm-call mandelbrot2 (apply llvm-vector crs2) (llvm-vector* ci mandel-size))))
             (for/list ((i mandel-size))
              (llvm-extract-element bits i)))))))
    (define dest (llvm-gep (llvm-get-param 2) 0 (llvm/ i 8)))
    (define shifted-bits 
      (for/list ((j 8) (b bits))
        (llvm-shl (llvm-zext b (llvm-int8-type))
                  (llvm-int (- 7 j) (llvm-int8-type)))))
    (define result
      (for/fold ((ans (llvm-int 0 (llvm-int8-type)))) ((b shifted-bits))
        (llvm-or ans b)))
    (llvm-store result dest))


  (llvm-ret-void)

  (void))
  

(let ((err (llvm-verify-module module)))
  (void
   (and err
        (error 'mandelbrot "Bad module: ~a, module" err))))


;(display (llvm-module-description module))
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
   
                                                                                                         
                                                                                                         





