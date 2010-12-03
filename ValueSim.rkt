;;; Libraries
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))

;;; Simulator

;;; Tests
;;(require rackunit)
;;(require rackunit)

;;; Environment
;; Core environment represented by an array of numbers
(define WORLD-SIZE 15)
(define environment
  (build-vector (expt WORLD-SIZE 2)
                (lambda (n)
                  (if (< (random 10) 1)
                      'obstacle
                      'normal))))

(define (fill-boundaries)
  (define (aux n)
    (cond [(>= n (expt WORLD-SIZE 2))(void)]
          [(or (zero? (modulo (+ n 1) WORLD-SIZE))
               (zero? (modulo n WORLD-SIZE))
               (= (quotient n WORLD-SIZE) 0)
               (= (quotient n WORLD-SIZE) (- WORLD-SIZE 1)))
           (vector-set! environment n 'obstacle)
           (aux (+ n 1))]
          [else
           (aux (+ n 1))]))
  (aux 0))

(fill-boundaries)

(define region-description (make-hash))
(hash-set! region-description 'obstacle 'O)
(hash-set! region-description 'normal '_)

;; make a vector of WORLD-SIZE x WORLD-SIZE elements
;; compare WORLD-SIZEorvig
;; must be able to create a random environment with some regions
;; 0.1 change for the block to be an obstacle

;;; Agent
(define-struct agent (body mind posn orientation) #:mutable)
(define-struct body (variables sensors motors physics))
(define-struct mind (perception value-system learning decision-making))
(define-struct posn (x y) #:mutable)
(define-struct orientation theta)

(define agent-view (make-hash))
(hash-set! agent-view 'N 'A)
(hash-set! agent-view 'S 'V)
(hash-set! agent-view 'E '>)
(hash-set! agent-view 'W '<)

;; Actions
(define (move)
  (let* ([theta (agent-orientation tested-agent)]
         [current-posn (agent-posn tested-agent)]
         [next-posn (+ current-posn (hash-ref orientation->movement theta))])
    (when (symbol=? (vector-ref environment next-posn) 'normal)
      (set-agent-posn! tested-agent next-posn))))

(define (turn-right)
  (let ([found #f])
    (define (aux dir)
      (cond [(and (empty? dir) found) (first orientations)]
            [(empty? dir) (error "could not find the requested direction~n" aux)]
            [found (first dir)]
            [(symbol=? (first dir) (agent-orientation tested-agent))
             (set! found #t)
             (aux (rest dir))]
            [else
             (aux (rest dir))]))
    (set-agent-orientation! tested-agent (aux orientations))))

(define (turn-left)
  (let ([orientations (reverse orientations)]
        [found #f])
    (define (aux dir)
      (cond [(and (empty? dir) found) (first orientations)]
            [(empty? dir) (error "could not find the requested direction~n" aux)]
            [found (first dir)]
            [(symbol=? (first dir) (agent-orientation tested-agent))
             (set! found #t)
             (aux (rest dir))]
            [else
             (aux (rest dir))]))
    (set-agent-orientation! tested-agent (aux orientations))))

;; Decision making
(define (random-as)
  (let* ([actions (body-motors (agent-body tested-agent))]
         [size (vector-length actions)]
         [n (random size)]
         [fn (vector-ref actions n)])
    fn))

;; Placing the agent in a valid random position
(define (valid-random-posn)
  (let ([posn (random (expt WORLD-SIZE 2))])
    (if (symbol=? (vector-ref environment posn) 'normal)
        posn
        (valid-random-posn))))

;;; Instantiations
(define simple-agent (agent
                      (body '(energy life)
                            null
                            (vector move turn-left turn-right)
                            'square)
                      (mind null
                            null
                            null
                            random-as)
                      (valid-random-posn)
                      'N))

;; TODO remove this global variable and introduce it as parameter in the relevant functions
(define tested-agent simple-agent)

(define orientations '(N E S W))

;; Movement model--the the position in the list corresponds to the orientations above
(define movements `(,(- WORLD-SIZE) +1 ,WORLD-SIZE -1))

;; Making it easier to look up the movement transition when knowing orientation
(define orientation->movement (make-hash))
(andmap (lambda (orientation movement)
       (hash-set! orientation->movement orientation movement))
     orientations movements)

;;; Simulator:
(define SIM-STEPS 1000)
;; 1) Ask environment for sensory data
;; 2) Ask agent for action
;; 3) Execute action
;; 4) -> 1
(define (run-simulation steps)
  (define (run-step n)
    (unless (>= n steps)
      ;;(void) ;; tell the agent its sensory data
      (let ([action-to-execute ((mind-decision-making (agent-mind tested-agent)))])
        (action-to-execute))
      (run-step (+ n 1))))
  (run-step 0))

;;; Data logging
;; For now will be logging following:
;;   position
;;   orientation
;;   SM data
;;   internal values
;; Provide a data structure (an array?) to store the logged data
(define-struct logged-piece (position orientation sensors motors internal-values))

;; position
;; every time that the agent chooses an action and returns to the simulator, log the data
;;; View
(define (print-environment)
  (define (aux n)
    (when (< n (expt WORLD-SIZE 2))
      (let* ([type (vector-ref environment n)]
             [print-symbol (if (= (agent-posn tested-agent) n)
                               (hash-ref agent-view (agent-orientation tested-agent))
                               (hash-ref region-description type))])
        (when (zero? (modulo n WORLD-SIZE))
          (printf "\n"))
        (printf "~a " print-symbol))
      (aux (+ n 1))))
  (aux 0))

;;; Run the simulation
(run-simulation SIM-STEPS)
(print-environment)

;;; Testing
;; initial world

;; agent

;; simulator

;; action execution

;; passing sensory values