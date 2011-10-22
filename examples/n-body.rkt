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
(require (for-syntax racket/base))
(require "../simple.rkt")
(require "../private/ffi/safe.rkt")
(require "../private/simple/parameters.rkt")
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


  (define (gep0 ptr . args)
    (apply gep ptr 0 args))

  (define (make-body x y z vx vy vz mass)
    (llvm-struct (llvm-vector x y z) (llvm-vector vx vy vz) mass))

  (define (body-position planet)
    (load (gep0 planet 0)))
  (define (body-velocity planet)
    (load (gep0 planet 1)))
  (define (body-mass planet)
    (load (gep0 system planet 2)))

  (define (set-body-position! planet val)
    (store val (gep0 planet 0)))
  (define (set-body-velocity! planet val)
    (store val (gep0 planet 1)))


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
  (llvm-define-global sun (gep0 system sun-index))
 
  (define (vector3 x)
    (llvm-vector x x x))
  (define (size v)
   (let ((v2 (* v v)))
    (+ (llvm-extract-element v2 0)
     (+ (llvm-extract-element v2 1)
        (llvm-extract-element v2 2)))))
  (define (+= ref value)
    (llvm:set ref (+ ref value)))
  (define (-= ref value)
    (llvm:set ref (- ref value)))



  (llvm-define-function offset-momentum
   (-> void)
   (define center (llvm:reference (llvm:box (vector3 0.0))))

   (for index 0 (< index size-of-system) (+ 1 index)
    (let ((planet (gep0 system index)))
      (+= center (* (body-velocity planet)
                    (vector3 (body-mass index))))))

   (set-body-velocity! (load sun)
                        (/ (- (vector3 0.0) center)
                           (vector3 +solar-mass+)))
   (return))
  

  (llvm-define-function energy
   (-> float)
   (define e (llvm:reference (llvm:box 0.0)))

   (for index 0 (< index size-of-system) (+ 1 index)
    (let ((planet (gep0 system index)))
     (+= e(* 0.5
            (* (body-mass index)
               (size (body-velocity planet)))))
     (for other-index (+ 1 index) (< other-index size-of-system) (+ 1 other-index)
      (let* ((other-planet (gep0 system other-index))
             (dpos (- (body-position planet) (body-position other-planet)))
             (dist (sqrt (size dpos))))
        (-= e (/ (* (body-mass index)
                    (body-mass other-index))
                 dist))))))
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


   (for* ((index size-of-system))
    (let* ((outer-planet (gep0 system index))
           (outer-pos (body-position outer-planet))
           (outer-mass (body-mass index)))
      (define vel (llvm:reference (llvm:box (body-velocity outer-planet))))
      (for inner-index (+ 1 index) (< inner-index size-of-system) (+ 1 inner-index)
        (let* ((inner-planet (gep0 system inner-index))
               (dpos (- outer-pos (body-position inner-planet)))
               (dist2 (size dpos))
               (mag   (/ +dt+ (* dist2 (sqrt dist2))))
               (dpos-mag (* dpos (vector3 mag)))
               (inner-mass  (body-mass inner-index)))
          (-= vel (* dpos-mag (vector3 inner-mass)))
          (set-body-velocity! inner-planet
            (+ (body-velocity inner-planet)
             (* dpos-mag (vector3 outer-mass))))))
       ;End of inner loop
       (set-body-velocity! outer-planet vel) 
       (set-body-position! outer-planet (+ outer-pos (* (vector3 +dt+) vel)))))
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
