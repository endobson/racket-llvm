#lang racket/base

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Imperative-style implementation based on the SBCL implementation by
;; Patrick Frankenberger and Juho Snellman, but using only native Scheme
;; idioms like 'named let' and 'do' special form.
;;
;; Contributed by Anthony Borla, then converted for Racket
;; by Matthew Flatt and Brent Fulgham
;; Converted to use LLVM Library by Eric Dobson

#|
Correct output N = 1000 is

-0.169075164
-0.169087605
|#

(require racket/cmdline
         racket/flonum)
(require "../simple.rkt")
(require "../private/ffi/safe.rkt")
(require "../private/simple/base.rkt")
(require "../private/short.rkt")
(require (only-in ffi/unsafe _double cblock->vector))

;; ------------------------------
;; define planetary masses, initial positions & velocity

(define +pi+ 3.141592653589793)
(define +days-per-year+ 365.24)

(define +solar-mass+ (* 4 +pi+ +pi+))

(define +dt+ 0.01)

(ll
 (llvm-define-module module
  #:exports (offset-momentum energy advance advance-n system)

  (define (make-body x y z vx vy vz mass)
    (llvm-struct (llvm-vector x y z) (llvm-vector vx vy vz)))

  (define (body-position planet)
    (llvm-extract-value planet 0))
  (define (body-velocity planet)
    (llvm-extract-value planet 1))
  (define (body-mass planet-index)
    (load (llvm-gep planet-masses 0 planet-index)))

  (define (set-body-position planet val)
    (llvm-insert-value planet val 0))
  (define (set-body-velocity planet val)
    (llvm-insert-value planet val 1))


  (llvm-define-global planet-masses 
    (llvm-constant-array
      +solar-mass+
      (* 9.54791938424326609e-4 +solar-mass+)
      (* 2.85885980666130812e-4 +solar-mass+)
      (*  4.36624404335156298e-05 +solar-mass+)
      (* 5.15138902046611451e-05 +solar-mass+)))

  (define sun-initial
    (make-body 0.0 0.0 0.0 0.0 0.0 0.0 +solar-mass+))

  (define jupiter-initial
    (make-body 4.84143144246472090
               -1.16032004402742839
               -1.03622044471123109e-1
               (* 1.66007664274403694e-3 +days-per-year+)
               (* 7.69901118419740425e-3 +days-per-year+)
               (* -6.90460016972063023e-5 +days-per-year+)
               (* 9.54791938424326609e-4 +solar-mass+)))

  (define saturn-initial
    (make-body 8.34336671824457987
               4.12479856412430479
               -4.03523417114321381e-1
               (* -2.76742510726862411e-3 +days-per-year+)
               (* 4.99852801234917238e-3 +days-per-year+)
               (* 2.30417297573763929e-5 +days-per-year+)
               (* 2.85885980666130812e-4 +solar-mass+)))

  (define uranus-initial
    (make-body 1.28943695621391310e1
               -1.51111514016986312e1
               -2.23307578892655734e-1
               (* 2.96460137564761618e-03 +days-per-year+)
               (* 2.37847173959480950e-03 +days-per-year+)
               (* -2.96589568540237556e-05 +days-per-year+)
               (*  4.36624404335156298e-05 +solar-mass+)))

  (define neptune-initial
    (make-body 1.53796971148509165e+01
               -2.59193146099879641e+01
               1.79258772950371181e-01
               (* 2.68067772490389322e-03 +days-per-year+)
               (* 1.62824170038242295e-03 +days-per-year+)
               (* -9.51592254519715870e-05 +days-per-year+)
               (* 5.15138902046611451e-05 +solar-mass+)))

  (define size-of-system 5)

  (llvm-define-global system 
    (llvm-constant-array
      sun-initial
      jupiter-initial
      saturn-initial
      uranus-initial
      neptune-initial))

  ;TODO llvm-define-global-contstant
  (define sun-index 0)
  (llvm-define-global sun (llvm-gep system 0 sun-index))
 
  (define (vector3 x)
    (llvm-vector x x x))
  (define (size v)
   (let ((v2 (* v v)))
    (+ (llvm-extract-element v2 0)
     (+ (llvm-extract-element v2 1)
        (llvm-extract-element v2 2)))))


  (llvm-define-function offset-momentum
   (-> void)
   (llvm-define-mutable center (vector3 0.0))

   (for index 0 (< index size-of-system) (+ 1 index)
    (let ((planet (load (gep system 0 index))))
      (set! center (+ center (* (body-velocity planet)
                                (vector3 (body-mass index)))))))

   (store 
    (set-body-velocity (load (load sun))
                        (/ (- (vector3 0.0) center)
                           (vector3 +solar-mass+)))
    (load sun))
   (return))
  

  (llvm-define-function energy
   (-> float)
   (llvm-define-mutable e 0.0)

   (for index 0 (< index size-of-system) (+ 1 index)
    (let ((planet (load (gep system 0 index))))
     (set! e
      (+ e (* 0.5 
            (* (body-mass index)
               (size (body-velocity planet))))))
     (for other-index (+ 1 index) (< other-index size-of-system) (+ 1 other-index)
      (let* ((other-planet (load (gep system 0 other-index)))
             (dpos (- (body-position planet) (body-position other-planet)))
             (dist (sqrt (size dpos))))
        (set! e (- e (/ (* (body-mass index)
                           (body-mass other-index))
                        dist)))))))
   (return e))


  (llvm-define-function advance-n
   ((n int) -> void)
   (for index 0 (< index n) (+ 1 index)
    (call advance))
   (return))

  (llvm-define-function advance
   (-> void)
   #:visibility 'hidden
   #:linkage 'private

    


   ;(for index 0 (< index size-of-system) (+ 1 index)
   (define mod-system
     (for/fold ((mod-system (load system))) ((index size-of-system))
      (let* ((outer-planet (llvm-extract-value mod-system index))
             (outer-pos (body-position outer-planet))
             (outer-mass (body-mass index)))
        (llvm-define-mutable vel (body-velocity outer-planet))
        ;(for inner-index (+ 1 index) (< inner-index size-of-system) (+ 1 inner-index)
        (let ((mod-system
          (let inner-loop ((inner-index (add1 index)) (mod-system mod-system))
            (if (#,< inner-index size-of-system) 
              (let* ((inner-planet (llvm-extract-value mod-system inner-index))
                     (dpos (- outer-pos (body-position inner-planet)))
                     (dist2 (size dpos))
                     (mag   (/ +dt+ (* dist2 (sqrt dist2))))
                     (dpos-mag (* dpos (vector3 mag)))
                     (inner-mass  (body-mass inner-index)))
                (set! vel (- vel (* dpos-mag (vector3 inner-mass))))
                (inner-loop
                  (add1 inner-index)
                  (llvm-insert-value
                   mod-system
                   (set-body-velocity inner-planet (+ (body-velocity inner-planet)
                                                    (* dpos-mag (vector3 outer-mass))))
                   inner-index)))
              mod-system))))
           ;End of inner loop
           (llvm-insert-value mod-system
             (set-body-position 
              (set-body-velocity outer-planet vel) 
              (+ outer-pos (* (vector3 +dt+) vel)))
             index)))))
   (store mod-system system)
   (return))
     
       
  
  ))
  

;(display (llvm-module-description module))
(llvm-assert-module-valid module)
(void (llvm:optimize-module module))
;(display (llvm-module-description module))
(define jit (llvm:create-jit module))
(define new-offset-momentum (llvm:extract-function jit offset-momentum))
(define new-energy (llvm:extract-function jit energy))
(define new-advance-n (llvm:extract-function jit advance-n))
(define new-system (llvm:extract-global jit system))

(let ([n (command-line #:args (n) (string->number n))])
  (new-offset-momentum)
  (printf "~a\n" (real->decimal-string (llvm:generic->double (new-energy)) 9))
  (new-advance-n (llvm:int32->generic n))
  (printf "~a\n" (real->decimal-string (llvm:generic->double (new-energy)) 9)))
