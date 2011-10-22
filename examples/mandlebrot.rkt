#lang racket/base

(require "../simple.rkt")
(require "../private/simple/parameters.rkt")
(require "../private/short.rkt")
(require ffi/unsafe)
(require racket/flonum)
(require racket/cmdline)
(require racket/fixnum)
(require racket/list)




(define mandel-size 8)
(define (mandel-vec type) (ll (vec type mandel-size)))

(define N (command-line #:args (n) (string->number n))) 
(define N.0 (fx->fl N)) 
(define 2/N (fl/ 2.0 N.0)) 
(define LIMIT-SQR 4.0) 
(define ITERATIONS 50) 


(define (llvm-and-vector vector)
  (for/fold ((acc (llvm-int 1 (llvm-int1-type))))
            ((i (llvm-get-vector-type-size (value->llvm-type vector))))
    (llvm-and acc (llvm-extract-element vector i))))

(define (mandel-vector v)
  (llvm-vector* (build-list mandel-size (lambda (i) v))))

(ll
 (llvm-define-module module
  #:exports (calculate-row)


 (llvm-define-function calculate-row
  ((ci float) (crs (ptr (arr float))) (block (ptr (arr i8))) -> void)

  (for i 0 (< i N) (+ i 8)
    (define bits
     (append*
      (for/list ((j (in-range 0 8 mandel-size)))
        (let* ((crs2 (for/list ((k mandel-size))
                        (load (gep crs 0 (+ i (+ j k)))))))
           (let ((bits (call mandelbrot (llvm-vector* crs2) (mandel-vector ci))))
             (for/list ((i mandel-size))
              (llvm-extract-element bits i)))))))
    (store
     (for/fold ((ans (llvm-int 0 i8)))
               ((j 8) (b bits))
        (or ans 
           (<< (zext b i8)
               (llvm-int #,(- 7 j) i8))))
     (gep block 0 (/ i 8))))

  (return))
 
 (llvm-define-function mandelbrot
  ((cr (mandel-vec float)) (ci (mandel-vec float)) -> (mandel-vec bool))
  #:visibility 'hidden
  #:linkage 'private

  (define LIMIT (mandel-vector 4.0))

  (define zr (llvm:reference (llvm:box (mandel-vector 0.0))))
  (define zi (llvm:reference (llvm:box (mandel-vector 0.0))))

  (define (compute-magnitude)
    (+ (* zr zr) (* zi zi)))

  (for i 0 (< i ITERATIONS) (+ i 1)
    (when (llvm-and-vector (> (compute-magnitude)  LIMIT)) 
        (return (mandel-vector #f)))
    (llvm:set-multiple (list zr zi)
      (+ (- (* zr zr) (* zi zi)) cr)
      (+ (* (mandel-vector 2.0) (* zr zi)) ci)))

  (return (< (compute-magnitude) LIMIT)))))
 
  
(llvm-assert-module-valid module)
(void (llvm:optimize-module module))
(define jit (llvm:create-jit module))



(define O (current-output-port))



(define Crs
  (let ([v (make-flvector N)])
   (begin0 v
    (for ([x (in-range N)])
      (flvector-set! v x (fl- (fl/ (fx->fl (fx* 2 x)) N.0) 1.5))))))
(define byte-array (make-bytes (ceiling (/ N 8))))
(define output-row (llvm:pointer->generic byte-array))
(define gen-Crs (llvm:pointer->generic (flvector->cpointer Crs)))
(define calc-row (llvm:extract-function jit calculate-row))


(fprintf O "P4\n~a ~a\n" N N)
(let loop-y ([y N])
  (let ([Ci (fl- (fl* 2/N (fx->fl y)) 1.0)])
   (let ((gen-Ci (llvm:double->generic Ci)))
     (calc-row gen-Ci gen-Crs output-row)
     (write-bytes byte-array O)
     (when (fx> y 1) (loop-y (fx- y 1))))))

