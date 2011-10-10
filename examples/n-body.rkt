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

(define-struct body (x y z vx vy vz mass)
  #:mutable #:transparent)

(define *sun*
  (make-body 0.0 0.0 0.0 0.0 0.0 0.0 +solar-mass+))

(define *jupiter*
  (make-body 4.84143144246472090
             -1.16032004402742839
             -1.03622044471123109e-1
             (* 1.66007664274403694e-3 +days-per-year+)
             (* 7.69901118419740425e-3 +days-per-year+)
             (* -6.90460016972063023e-5 +days-per-year+)
             (* 9.54791938424326609e-4 +solar-mass+)))

(define *saturn*
  (make-body 8.34336671824457987
             4.12479856412430479
             -4.03523417114321381e-1
             (* -2.76742510726862411e-3 +days-per-year+)
             (* 4.99852801234917238e-3 +days-per-year+)
             (* 2.30417297573763929e-5 +days-per-year+)
             (* 2.85885980666130812e-4 +solar-mass+)))

(define *uranus*
  (make-body 1.28943695621391310e1
             -1.51111514016986312e1
             -2.23307578892655734e-1
             (* 2.96460137564761618e-03 +days-per-year+)
             (* 2.37847173959480950e-03 +days-per-year+)
             (* -2.96589568540237556e-05 +days-per-year+)
             (*  4.36624404335156298e-05 +solar-mass+)))

(define *neptune*
  (make-body 1.53796971148509165e+01
             -2.59193146099879641e+01
             1.79258772950371181e-01
             (* 2.68067772490389322e-03 +days-per-year+)
             (* 1.62824170038242295e-03 +days-per-year+)
             (* -9.51592254519715870e-05 +days-per-year+)
             (* 5.15138902046611451e-05 +solar-mass+)))

(define *system* (list *sun* *jupiter* *saturn* *uranus* *neptune*))

(ll
 (llvm-define-module module
  #:exports (offset-momentum energy advance advance-n system)

  (define make-body llvm-constant-array)
  (define (ll-set-body-x! planet-pointer val)
    (store val (gep planet-pointer 0 0)))
  (define (ll-set-body-y! planet-pointer val)
    (store val (gep planet-pointer 0 1)))
  (define (ll-set-body-z! planet-pointer val)
    (store val (gep planet-pointer 0 2)))
  (define (ll-set-body-vx! planet-pointer val)
    (store val (gep planet-pointer 0 3)))
  (define (ll-set-body-vy! planet-pointer val)
    (store val (gep planet-pointer 0 4)))
  (define (ll-set-body-vz! planet-pointer val)
    (store val (gep planet-pointer 0 5)))

  (define (ll-body-x planet-pointer)
    (load (gep planet-pointer 0 0)))
  (define (ll-body-y planet-pointer)
    (load (gep planet-pointer 0 1)))
  (define (ll-body-z planet-pointer)
    (load (gep planet-pointer 0 2)))
  (define (ll-body-vx planet-pointer)
    (load (gep planet-pointer 0 3)))
  (define (ll-body-vy planet-pointer)
    (load (gep planet-pointer 0 4)))
  (define (ll-body-vz planet-pointer)
    (load (gep planet-pointer 0 5)))
  (define (ll-body-mass planet-pointer)
    (load (gep planet-pointer 0 6)))

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
 

  (llvm-define-function offset-momentum
   (-> void)
   (llvm-define-mutable px 0.0)
   (llvm-define-mutable py 0.0)
   (llvm-define-mutable pz 0.0)

   (for index 0 (< index size-of-system) (+ 1 index)
    (let ((planet (gep system 0 index)))
     (set! px (+ px (* (ll-body-vx planet) (ll-body-mass planet))))
     (set! py (+ py (* (ll-body-vy planet) (ll-body-mass planet))))
     (set! pz (+ pz (* (ll-body-vz planet) (ll-body-mass planet))))))

   (ll-set-body-vx! (load sun) (/ (- 0.0 px) +solar-mass+))
   (ll-set-body-vy! (load sun) (/ (- 0.0 py) +solar-mass+))
   (ll-set-body-vz! (load sun) (/ (- 0.0 pz) +solar-mass+))
   (return))
  

  (llvm-define-function energy
   (-> float)
   (llvm-define-mutable e 0.0)

   (for index 0 (< index size-of-system) (+ 1 index)
    (let ((planet (gep system 0 index)))
     (set! e
      (+ e (* 0.5 
            (* (ll-body-mass planet)
               (+ (+ (* (ll-body-vx planet) (ll-body-vx planet))
                     (* (ll-body-vy planet) (ll-body-vy planet)))
                     (* (ll-body-vz planet) (ll-body-vz planet)))))))
     (for other-index (+ 1 index) (< other-index size-of-system) (+ 1 other-index)
      (let* ((other-planet (gep system 0 other-index))
             (dx   (- (ll-body-x planet) (ll-body-x other-planet)))
             (dy   (- (ll-body-y planet) (ll-body-y other-planet)))
             (dz   (- (ll-body-z planet) (ll-body-z other-planet)))
             (dist (sqrt (+ (+ (* dx dx) (* dy dy)) (* dz dz)))))
        (set! e (- e (/ (* (ll-body-mass planet) (ll-body-mass other-planet)) dist)))))))
   (return e))


  (llvm-define-function advance-n
   ((n int) -> void)
   (for index 0 (< index n) (+ 1 index)
    (call advance))
   (return))

  (llvm-define-function advance
   (-> void)
   (for index 0 (< index size-of-system) (+ 1 index)
    (let* ((outer-planet (gep system 0 index))
           (outer-x (ll-body-x outer-planet))
           (outer-y (ll-body-y outer-planet))
           (outer-z (ll-body-z outer-planet))
           (outer-mass (ll-body-mass outer-planet)))
      (llvm-define-mutable vx (ll-body-vx outer-planet))
      (llvm-define-mutable vy (ll-body-vy outer-planet))
      (llvm-define-mutable vz (ll-body-vz outer-planet))
      (for inner-index (+ 1 index) (< inner-index size-of-system) (+ 1 inner-index)
        (let* ((inner-planet (gep system 0 inner-index))
               (dx    (- outer-x (ll-body-x inner-planet)))
               (dy    (- outer-y (ll-body-y inner-planet)))
               (dz    (- outer-z (ll-body-z inner-planet)))
               (dist2 (+ (+ (* dx dx) (* dy dy)) (* dz dz)))
               (mag   (/ +dt+ (* dist2 (sqrt dist2))))
               (dxmag (* dx mag))
               (dymag (* dy mag))
               (dzmag (* dz mag))
               (inner-mass    (ll-body-mass inner-planet)))
          (ll-set-body-vx! inner-planet (+ (ll-body-vx inner-planet) (* dxmag outer-mass)))
          (ll-set-body-vy! inner-planet (+ (ll-body-vy inner-planet) (* dymag outer-mass)))
          (ll-set-body-vz! inner-planet (+ (ll-body-vz inner-planet) (* dzmag outer-mass)))
          (set! vx (- vx (* dxmag inner-mass)))
          (set! vy (- vy (* dymag inner-mass)))
          (set! vz (- vz (* dzmag inner-mass)))))
      ;End of inner loop
      (begin (ll-set-body-vx! outer-planet vx)
             (ll-set-body-vy! outer-planet vy)
             (ll-set-body-vz! outer-planet vz)
             (ll-set-body-x! outer-planet (+ outer-x (* +dt+ vx)))
             (ll-set-body-y! outer-planet (+ outer-y (* +dt+ vy)))
             (ll-set-body-z! outer-planet (+ outer-z (* +dt+ vz))))))
   (return))
     
       
  
  ))
  

;; -------------------------------
(define (orig-offset-momentum)
  (let loop-i ([i *system*] [px 0.0] [py 0.0] [pz 0.0])
    (if (null? i)
      (begin
        (set-body-vx! (car *system*) (fl/ (fl- 0.0 px) +solar-mass+))
        (set-body-vy! (car *system*) (fl/ (fl- 0.0 py) +solar-mass+))
        (set-body-vz! (car *system*) (fl/ (fl- 0.0 pz) +solar-mass+)))
      (let ([i1 (car i)])
        (loop-i (cdr i)
                (fl+ px (fl* (body-vx i1) (body-mass i1)))
                (fl+ py (fl* (body-vy i1) (body-mass i1)))
                (fl+ pz (fl* (body-vz i1) (body-mass i1))))))))

;; -------------------------------
(define (orig-energy)
  (let loop-o ([o *system*] [e 0.0])
    (if (null? o)
      e
      (let* ([o1 (car o)]
             [e (+ e (fl* 0.5 
                          (fl* (body-mass o1)
                               (fl+ (fl+ (fl* (body-vx o1) (body-vx o1))
                                         (fl* (body-vy o1) (body-vy o1)))
                                    (fl* (body-vz o1) (body-vz o1))))))])
        (let loop-i ([i (cdr o)] [e e])
          (if (null? i)
            (loop-o (cdr o) e)
            (let* ([i1   (car i)]
                   [dx   (fl- (body-x o1) (body-x i1))]
                   [dy   (fl- (body-y o1) (body-y i1))]
                   [dz   (fl- (body-z o1) (body-z i1))]
                   [dist (flsqrt (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz)))]
                   [e    (fl- e (fl/ (fl* (body-mass o1) (body-mass i1)) dist))])
              (loop-i (cdr i) e))))))))

;; -------------------------------
(define (orig-advance)
  (let loop-o ([o *system*])
    (when (pair? o)
      (let* ([o1  (car o)]
             [o1x (body-x o1)]
             [o1y (body-y o1)]
             [o1z (body-z o1)]
             [om  (body-mass o1)])
        (let loop-i ([i  (cdr o)]
                     [vx (body-vx o1)]
                     [vy (body-vy o1)]
                     [vz (body-vz o1)])
          (if (pair? i)
            (let* ([i1    (car i)]
                   [dx    (fl- o1x (body-x i1))]
                   [dy    (fl- o1y (body-y i1))]
                   [dz    (fl- o1z (body-z i1))]
                   [dist2 (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz))]
                   [mag   (fl/ +dt+ (fl* dist2 (flsqrt dist2)))]
                   [dxmag (fl* dx mag)]
                   [dymag (fl* dy mag)]
                   [dzmag (fl* dz mag)]
                   [im    (body-mass i1)])
              (set-body-vx! i1 (fl+ (body-vx i1) (fl* dxmag om)))
              (set-body-vy! i1 (fl+ (body-vy i1) (fl* dymag om)))
              (set-body-vz! i1 (fl+ (body-vz i1) (fl* dzmag om)))
              (loop-i (cdr i)
                      (fl- vx (fl* dxmag im))
                      (fl- vy (fl* dymag im))
                      (fl- vz (fl* dzmag im))))
            (begin (set-body-vx! o1 vx)
                   (set-body-vy! o1 vy)
                   (set-body-vz! o1 vz)
                   (set-body-x! o1 (fl+ o1x (fl* +dt+ vx)))
                   (set-body-y! o1 (fl+ o1y (fl* +dt+ vy)))
                   (set-body-z! o1 (fl+ o1z (fl* +dt+ vz)))))))
      (loop-o (cdr o)))))

;; -------------------------------

;(display (llvm-module-description module))
(llvm-assert-module-valid module)
(void (llvm:optimize-module module))
(define jit (llvm:create-jit module))
(define new-offset-momentum (llvm:extract-function jit offset-momentum))
(define new-energy (llvm:extract-function jit energy))
(define new-advance (llvm:extract-function jit advance))
(define new-advance-n (llvm:extract-function jit advance-n))
(define new-system (llvm:extract-global jit system))

(let ([n (command-line #:args (n) (string->number n))])

  (printf "~a\n" (real->decimal-string (llvm:generic->double (new-energy)) 9))
  (orig-offset-momentum)
  (new-offset-momentum)
  (printf "~a\n" (real->decimal-string (llvm:generic->double (new-energy)) 9))
  (new-advance-n (llvm:int32->generic n))
  (printf "~a\n" (real->decimal-string (llvm:generic->double (new-energy)) 9))
  )
