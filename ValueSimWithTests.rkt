;;; New Representation Prototype
;; World Builder
(define-struct posn (x y) #:mutable #:transparent)


(define-struct tile (color temperature object-on-top traversable?)
  #:mutable #:transparent)

(define-struct color (r g b)
  #:mutable #:transparent)


;; Levers
(define (lever #:color [color #f]
               #:temperature [temperature #f]
               #:pullable? [pullable? #t])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))  
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (pullable? . ,pullable?))))

;; Buttons
(define (button #:color [color #f]
                #:temperature [temperature #f]
                #:pushable? [pushable? #t])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))  
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (pushable? . ,pushable?))))

;; Rocks
(define (rock #:color [color #f]
              #:temperature [temperature #f]
              #:movable? [movable? empty])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (when (empty? movable?)
    (if (zero? (random 2))
        (set! movable? #t)
        (set! movable? #f)))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (movable? . ,movable?))))

;; Battery pack
(define (battery-pack #:color [color #f]
                      #:temperature [temperature #f]
                      #:movable? [movable? empty])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (when (empty? movable?)
    (if (zero? (random 2))
        (set! movable? #t)
        (set! movable? #f)))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (movable? . ,movable?))))

(define (build-environment world-size)
  (let ([environment #f])
    (define (fill-boundaries)
      (define (aux n)
        (cond [(>= n (expt world-size 2)) environment]
              [(or (zero? (modulo (+ n 1) world-size))
                   (zero? (modulo n world-size))
                   (= (quotient n world-size) 0)
                   (= (quotient n world-size) (- world-size 1)))
               (set-tile-object-on-top! (vector-ref environment n)
                                        (rock #:movable? #f))
               (aux (+ n 1))]
              [else
               (aux (+ n 1))]))
      (aux 0))
    (set! environment
          (build-vector (expt world-size 2)
                        (lambda (n)
                          (if (< (random 100) 0)
                              (tile (make-color 100 100 100)  0 0 0)
                              (tile (make-color 19 201 19)  0 0 0)))))
    ;;environment
    (fill-boundaries)))


;; function converting the number of grid into x and y coordinates of the agent
;; assume a square environment
;; position->coordinates : N x N -> posn
(define (position->coordinates world-size position)
  (make-posn (remainder position world-size)
             (quotient position world-size)))


;;; Tests
(define (test-all)
  (load "Tests.rkt"))

(define-syntax my-time
  (syntax-rules ()
    [(my-time e)
     (let ([begin-time (current-milliseconds)])
       (begin
         e
         (- (current-milliseconds) begin-time)))]))

(define (clear)
  (for ([i (in-range 5)])
       (collect-garbage)))