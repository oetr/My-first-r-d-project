;;; New Representation Prototype
;; World Builder
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
               (pullable? . ,pullable?)
               (name . lever))))

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
               (pushable? . ,pushable?)
               (name . button))))

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
               (movable? . ,movable?)
               (name . rock))))

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
               (movable? . ,movable?)
               (name . battery-pack))))

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
                          (tile (make-color 19 201 19) 0 #f 0))))
  (fill-boundaries)))

(define WORLD-SIZE 5)
(define env (build-environment WORLD-SIZE))
(define movements (list->vector `(,(- WORLD-SIZE) +1 ,WORLD-SIZE -1)))

;; Agent definitions
(define-struct agent (position orientation energy life) #:mutable #:transparent)
(define-struct posn (x y) #:mutable #:transparent)

(define (place-agent-randomly agent environment world-size)
  (let ([position (random (expt world-size 2))])
    (if (tile-object-on-top (vector-ref environment position))
        (place-agent-randomly agent environment world-size)
        (begin
;;          (printf "agent placement: OK\n")
          (make-agent position
                      (random 4)
                      (agent-energy agent)
                      (agent-life agent))))))

(define (place-agent agent environment position orientation)
  (cond [(> position (vector-length environment))
;;         (printf "could not place~n")
         agent]
        [(tile-object-on-top (vector-ref environment position))
;;         (printf "could not place~n")
         agent]
        [else (make-agent position
                          orientation
                          (agent-energy agent)
                          (agent-life agent))]))

(define A (make-agent 0 0 0 0))
(set! A (place-agent-randomly A env WORLD-SIZE))

;; Actions
(define (turn-left! agent environment)
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 3)
                                 4)))

(define (turn-right! agent environment)
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 1)
                                 4)))

(define (move! agent environment)
  (let ([next-position (+ (agent-position agent)
                          (vector-ref movements (agent-orientation agent)))])
    (unless (tile-object-on-top (vector-ref environment next-position))
      (set-agent-position! agent next-position))))

                      
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

(define (random-as probabilities x)
  (when (empty? probabilities)
    (error "probabilities should not be empty" random-as))
  (let ([xnu (- x (car probabilities))])
    (if (< xnu 0)
        0
        (1+ (random-as (cdr probabilities) xnu)))))

(define (1+ x)
  (+ x 1))


;;; Printing
(define (print-environment agent environment world-size)
  (define (aux n)
    (when (< n (sqr world-size))
      (let* ([tile (vector-ref environment n)]
             [symbol (print-symbol n tile agent)])
        (when (zero? (modulo n WORLD-SIZE))
          (printf "\n"))
        (printf "~a " symbol))
      (aux (+ n 1))))
  (aux 0)
  (printf "~n"))

(define (print-symbol n tile agent)
  (cond [(= n (agent-position agent)) 'A]
        [(false? (tile-object-on-top tile)) '_]
        [else 'X]))

(define (print-world)
  (print-environment A env WORLD-SIZE))