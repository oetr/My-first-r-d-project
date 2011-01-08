;;(load "/Users/petr/Dropbox/Libraries/Racket/utils.rkt")
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
(require racket/date)

;;; Objects
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

;; Battery packs
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

;;; Environment
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
                          (if (> (random 30) 1)
                              (tile (make-color 19 201 19) 15 #f 0)
                              (tile (make-color 19 201 19) 15 (rock) 0)))))
    (fill-boundaries)))

;;; Agent
(define-struct agent (position orientation energy life fn) #:mutable #:transparent)
(define-struct posn (x y) #:mutable #:transparent)

(define (place-agent-randomly agent environment world-size)
  (let ([position (random (expt world-size 2))])
    (if (tile-object-on-top (& environment position))
        (place-agent-randomly agent environment world-size)
        (begin
          (make-agent position
                      (random 4)
                      (agent-energy agent)
                      (agent-life agent)
                      (agent-fn agent))))))

;; manually place the agent
(define (place-agent agent environment position orientation)
  (cond [(> position (vector-length environment)) agent]
        [(tile-object-on-top (& environment position)) agent]
        [else
         (make-agent position
                     orientation
                     (agent-energy agent)
                     (agent-life agent)
                     (agent-fn agent))]))

;; Some simple decision making functions
(define (random-as agent actions)
  (lambda (percepts)
    (let ([decision (& actions (random (vector-length actions)))])
      (values (vector 0) decision))))

;;; Actions
(define (turn-left! agent environment movements)
  (set-agent-energy! agent (reduce-energy (agent-energy agent) 0.5))
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 3)
                                 4)))

(define (turn-right! agent environment movements)
  (set-agent-energy! agent (reduce-energy (agent-energy agent) 0.5))
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 1)
                                 4)))

(define (move! agent environment movements)
  (let ([next-position (+ (agent-position agent)
                          (& movements (agent-orientation agent)))])
    ;; reduce energy
    (set-agent-energy! agent (reduce-energy (agent-energy agent) 10))
    (unless (tile-object-on-top (& environment next-position))
      (set-agent-position! agent next-position))))

(define (do-nothing! agent environment movements)
  (set-agent-energy! agent (reduce-energy (agent-energy agent) 0.01))
  (void))

(define (reduce-energy current-amount amount)
  (let ([next-energy (- current-amount amount)])
    (if (< next-energy 0)
        0
        next-energy)))

(define actions (vector move! turn-left! turn-right! do-nothing!))


;;; Sensors
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
    (define (turn-left orientation)
      (remainder (+ orientation 3) 4))
    (define (turn-right orientation)
      (remainder (+ orientation 1) 4))
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
             [start (posn+ position front)])
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

;; produces a vector of values
(define (sense agent environment movements)
  (vector-append
   ;; transmitting energy
   (vector
    (agent-energy agent)
    ;; life
    (agent-life agent))
   ;; temperature
   (compute-surrounding-temperatures agent environment)
   ;; vision
   (call-with-values
       (lambda () (vector->values
              (vector-map
               (lambda (color) (vector (color-r color)
                                  (color-g color)
                                  (color-b color)))
               (compute-vision agent environment movements))))
     vector-append)))

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

;;; Data logger
(define-struct log (percepts value-system-label decision position orientation)
  #:transparent #:mutable )

(define data #())

(define (log-data! percepts value-system-label decision agent data n)
  (! data n
     (make-log percepts value-system-label decision (agent-position agent)
               (agent-orientation agent))))

(define (print-comma-separated data a-file separator end)
  (if (or (number? data) (procedure? data))
      (fprintf a-file "~a~a" data end)
      (begin
        (let ([length (vector-length data)])
          (define (print-comma-separated-aux i)
            (when (< i length)
              (fprintf a-file "~a" (& data i))
              (if (= i (- length 1))
                  (fprintf a-file "~a" end)
                  (fprintf a-file "~a" separator))
              (print-comma-separated-aux (+ i 1))))
          (print-comma-separated-aux 0)))))

;; data format
;; 0 1 2 3 4 ...
;; p o x y a e l
;; pos orienation x y action energy life
(define (save-log data world-size)
  (let ([file-out #f])
    (set! file-out (data-file-open "/Users/petr/Dropbox/rnd1/Simulator/Data/"))
    (printf "~a~n" file-out)
    (vector-map
     (lambda (log)
       (let ([pos (position->coordinates world-size (log-position log))])
         (print-comma-separated (log-position log) file-out ", " ", ")
         (print-comma-separated (log-orientation log) file-out "," ", ")
         (print-comma-separated (posn-x pos) file-out ", " ", ")
         (print-comma-separated (posn-y pos) file-out ", " ", ")
         (print-comma-separated (vector-member (log-decision log) actions)
                                file-out ", " ", ")
         (print-comma-separated (log-percepts log) file-out ", " ", ")
         (print-comma-separated (log-value-system-label log) file-out ", " "\n")))
     data)
    (data-file-close file-out)))

;;; Saving logs into a file
(define (timestamp)
  (let ([now (current-date)]
        [n (open-output-string)])
    (fprintf n "ValueSim-~a-~a-~a-~a-~a"
             (date-year now)
             (date-month now)
             (date-hour now)
             (date-minute now)
             (date-second now))
    (get-output-string n)))

(define (data-file-open dir)
  (open-output-file (string-append dir
                                   (timestamp)
                                   ".txt")
                    #:mode 'text
                    #:exists 'replace))

(define (data-file-close file-out)
  (close-output-port file-out))

;;; Run simulator
(define (update-world! decision agent environment movements)
  (decision agent environment movements))

(define (agent-live agent percepts)
  ((agent-fn agent) percepts))
  
(define (run-simulation agent environment movements data steps)
  (let ([percepts #f]
        [value-system-label #f]
        [decision #f])
    (define (run-simulation-aux n)
      (if (< n steps)
          (begin
            ;; sense
            (set! percepts (sense agent environment movements))
            ;; let the agent make decision combining new percepts
            (set!-values (value-system-label decision)
                         (agent-live agent percepts))
            ;;        (printf "decision: ~a, label: ~a~n" decision value-system-label)
            ;; log data
            (log-data! percepts value-system-label decision agent data n)
            ;; TODO test that the content of all the vectors is copied
            ;; and not the pointers
            ;; update the world
            (update-world! decision agent environment movements)
            (run-simulation-aux (+ n 1)))
          data))
    (set! data (make-vector steps #f))
    (run-simulation-aux 0)))

(define (step)
  (run-simulation A env movements data 1))

(define (run n)
  (set! data (run-simulation A env movements data n))
  (save-log data WORLD-SIZE)
  (void))

;;; Instantiate environment and agent
;; let's build an environment
(define WORLD-SIZE 200)
(define env (build-environment WORLD-SIZE))
(define movements (list->vector `(,(- WORLD-SIZE) +1 ,WORLD-SIZE -1)))
(define A (make-agent 0 0 3000 3000 void))
(set-agent-fn! A (random-as A actions))

(define B (make-agent 0 0 100 100 void))
(set-agent-fn! B (random-as B actions))

(set! A (place-agent-randomly A env WORLD-SIZE))

;; Create a simulator
(define STEPS 10)

;;; Tests
(define (test-all)
  (load "Tests.rkt"))

;; (define (random-as probabilities x)
;;   (when (empty? probabilities)
;;     (error "probabilities should not be empty" random-as))
;;   (let ([xnu (- x (car probabilities))])
;;     (if (< xnu 0)
;;         0
;;         (1+ (random-as (cdr probabilities) xnu)))))

;;; Visualization
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

;; Buggy version of the function
;; works only for the unary operator case
(define (vector-apply f v)
  (let ([length (vector-length v)])
    (define (vector-apply-aux result n)
      (if (= n length)
          result
          (vector-apply-aux (f result (& v n)) (+ n 1))))
    (if (zero? length)
        (error "No arguments provided in " vector-apply)
        (vector-apply-aux (& v 0) 1))))


;;(require macro-debugger/expand)
;;(require macro-debugger/stepper-text)
