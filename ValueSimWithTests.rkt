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
               (set-tile-object-on-top! (& environment n)
                                        (rock #:movable? #f))
               (aux (+ n 1))]
              [else
               (aux (+ n 1))]))
      (aux 0))
    (set! environment
          (build-vector (expt world-size 2)
                        (lambda (n)
                          (tile (make-color 19 201 19) 15 #f 0))))
  (fill-boundaries)))

(define WORLD-SIZE 10)
(define env (build-environment WORLD-SIZE))
(define movements (list->vector `(,(- WORLD-SIZE) +1 ,WORLD-SIZE -1)))

;; Agent definitions
(define-struct agent (position orientation energy life) #:mutable #:transparent)
(define-struct posn (x y) #:mutable #:transparent)

(define (place-agent-randomly agent environment world-size)
  (let ([position (random (expt world-size 2))])
    (if (tile-object-on-top (& environment position))
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
        [(tile-object-on-top (& environment position))
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

(define (turn-left orientation)
  (remainder (+ orientation 3) 4))

(define (turn-right orientation)
  (remainder (+ orientation 1) 4))

(define (turn-right! agent environment)
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 1)
                                 4)))

(define (move! agent environment movements)
  (let ([next-position (+ (agent-position agent)
                          (& movements (agent-orientation agent)))])
    (unless (tile-object-on-top (& environment next-position))
      (set-agent-position! agent next-position))))

;; Sensors
(define (compute-surrounding-temperatures agent environment)
  (define (compute-temperature pos)
    (let* ([a-tile (& environment pos)]
           [object-on-top (tile-object-on-top a-tile)])
      (if object-on-top
          (hash-ref object-on-top 'temperature)
          (tile-temperature a-tile))))
  (define (surrounding-tiles)
    (let* ([position (agent-position agent)]
           [north-position (+ position (& movements 0))]
           [south-position (+ position (& movements 2))])
      (vector
       ;; upper 3 tiles
       (- north-position 1) north-position (+ north-position 1)
       ;; middle tiles
       (- position 1) position (+ position 1)
       ;; middle tiles
       (- south-position 1) south-position (+ south-position 1))))
  (vector-map compute-temperature (surrounding-tiles)))

(define (compute-vision agent environment movements)
  (let ([world-size (sqrt (vector-length environment))])
    (define (position-out-of-bounds posn)
      (or (< (posn-x posn) 0) (>= (posn-x posn) world-size)
          (< (posn-y posn) 0) (>= (posn-y posn) world-size)))    
    (define (compute-color posn)
      (if (position-out-of-bounds posn)
          (make-color 0 0 0)
          (let* ([a-tile (& environment (coordinates->position world-size posn))]
                 [object-on-top (tile-object-on-top a-tile)])
            (if object-on-top
                (hash-ref object-on-top 'color)
                (tile-color a-tile)))))
    (define (visible-tiles)
      ;; return 9 positions that are in front of the agent
      (let* ([position (position->coordinates world-size
                                              (agent-position agent))]
             [orientation (agent-orientation agent)]
             [front (& x-y-movements orientation)]
             [left (& x-y-movements (turn-left orientation))]
             [right (& x-y-movements (turn-right orientation))]
             [start (sum-posn position front)])
        (vector
         ;; first row from left to right
         (posn+ start (posn+ left left))
         (posn+ start left)
         start
         (posn+ start right)
         (posn+ start (posn+ right right))
         ;; second row
         (posn+ start (posn+ front left))
         (posn+ start front)
         (posn+ start (posn+ front right))
         ;; third row
         (posn+ start (posn+ front front)))))
    (vector-map compute-color (visible-tiles))))

(define (sense agent environment)
  (vector
   ;; transmitting energy
   (agent-energy agent)
   ;; life
   (agent-life agent)
   ;; temperature
   (compute-surrounding-temperatures agent environment)
   ;; vision
   (vector-map (lambda (color) (vector (color-r color)
                                  (color-g color)
                                  (color-b color)))
               (compute-vision agent environment movements))
   ))

(define (print-temperatures environment)
  (define (aux n)
    (when (< n (vector-length environment))
      (let* ([tile (& environment n)]
             [object-on-top (tile-object-on-top tile)]
             [symbol #f])
        (if object-on-top
            (set! symbol (hash-ref object-on-top 'temperature))
            (set! symbol (tile-temperature tile)))
        (when (zero? (modulo n WORLD-SIZE))
          (printf "\n"))
        (printf "~a " symbol))
      (aux (+ n 1))))
  (aux 0)
  (printf "~n"))


;; function converting the number of grid into x and y coordinates of the agent
;; assume a square environment
;; position->coordinates : N x N -> posn
(define (position->coordinates world-size position)
  (make-posn (remainder position world-size)
             (quotient position world-size)))

(define (coordinates->position world-size posn)
  (+ (posn-x posn) (* (posn-y posn) world-size)))

(define x-y-movements (vector (make-posn 0 -1) (make-posn 1 0)
                                     (make-posn 0 1) (make-posn -1 0)))

(define (posn+ posn1 posn2)
  (make-posn (+ (posn-x posn1) (posn-x posn2))
             (+ (posn-y posn1) (posn-y posn2))))

(define (move-in-x-y posn orientation coordinate-movements)
  (posn+ posn (& coordinate-movements orientation)))

;;; Tests
(define (test-all)
  (load "Tests.rkt"))

(define (random-as probabilities x)
  (when (empty? probabilities)
    (error "probabilities should not be empty" random-as))
  (let ([xnu (- x (car probabilities))])
    (if (< xnu 0)
        0
        (1+ (random-as (cdr probabilities) xnu)))))

;;; Printing
(define (print-environment agent environment world-size)
  (define (aux n)
    (when (< n (sqr world-size))
      (let* ([tile (& environment n)]
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
