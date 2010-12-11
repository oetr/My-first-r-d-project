;;; Libraries
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
(require rackunit)

;;; Simulator

;;; Tests

;;; Environment
;; Core environment represented by an array of numbers
(define WORLD-SIZE 15)
(define environment #f)

(define (build-environment world-size)
  (let ([environment #f])
    (define (fill-boundaries)
      (define (aux n)
        (cond [(>= n (expt world-size 2)) environment]
              [(or (zero? (modulo (+ n 1) world-size))
                   (zero? (modulo n world-size))
                   (= (quotient n world-size) 0)
                   (= (quotient n world-size) (- world-size 1)))
               (vector-set! environment n 'obstacle)
               (aux (+ n 1))]
              [else
               (aux (+ n 1))]))
      (aux 0))
    (set! environment
          (build-vector (expt world-size 2)
                        (lambda (n)
                          (if (< (random 100) 3)
                              'obstacle
                              'normal))))
    (fill-boundaries)))

(set! environment (build-environment WORLD-SIZE))

(define region-description (make-hash))
(hash-set! region-description 'obstacle 'O)
(hash-set! region-description 'normal '_)

;;; Agent
(define-struct agent (body mind posn orientation) #:mutable)
(define-struct body (variables sensors motors physics))
(define-struct mind (perception value-system learning decision-making))
(define-struct posn (x y) #:mutable #:transparent)

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
      (let ([variables (body-variables (agent-body tested-agent))])
        (set-agent-posn! tested-agent next-posn)
        (hash-set! variables 'life (- (hash-ref variables 'life)
                                      0.001))
        (hash-set! variables 'energy (- (hash-ref variables 'energy)
                                        0.01))))))

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
                      (body (make-hash '((life . 5) (energy . 10)))
                            null
                            (vector move turn-left turn-right)
                            'square)
                      (mind null
                            null
                            null
                            random-as)
                      (valid-random-posn)
                      'N))

;; TODO remove this global variable and introduce it as parameter in every function
;; that uses it
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
(define (run-simulation steps)
  (define (run-step n)
    (when (< n steps)
      ;;(void) ;; tell the agent its sensory data
      (let ([action-to-execute ((mind-decision-making (agent-mind tested-agent)))])
        ;; Update the world by executing the action of the agent
        (action-to-execute)
        ;; log results of the action
        (let ([a-log-cell (vector-ref log-data n)]
              [a-posn (agent-posn tested-agent)]
              [an-orientation (agent-orientation tested-agent)])
          (set-log-cell-posn! a-log-cell a-posn)
          (set-log-cell-orientation! a-log-cell an-orientation)))
      ;; Update the state of the agent's body
      ;; Energy
      ;; Satiation
      (run-step (+ n 1))))
  (set! log-data (build-vector steps (lambda (n)
                                       (log-cell
                                        (posn 0 0)
                                        0 0 0 0)))) ;; instantiated with zero
  (run-step 0))

(define (run-simulation-with-print steps)
  (define (run-step n)
    (unless (>= n steps)
      ;;(void) ;; tell the agent its sensory data
      (let ([action-to-execute ((mind-decision-making (agent-mind tested-agent)))])
        (action-to-execute)
        ;; log results of the action
        (let ([a-log-cell (vector-ref log-data n)]
              [a-posn (agent-posn tested-agent)]
              [an-orientation (agent-orientation tested-agent)])
          (set-log-cell-posn! a-log-cell a-posn)
          (set-log-cell-orientation! a-log-cell an-orientation)))
      ;; show live the movements of the agent
      (print-environment)
      (sleep 0.2)
      (run-step (+ n 1))))
  (set! log-data (build-vector steps (lambda (n)
                                       (log-cell
                                        (posn 0 0)
                                        0 0 0 0)))) ;; instantiated with zero
  (run-step 0))

;;; Data logging
(define SIM-STEPS 1000)

(define-struct log-cell (posn orientation sensors motors internal-values) #:mutable)

(define log-data empty)

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
  (aux 0)
  (printf "~n"))

(define (print-log-data data)
  (vector-map (lambda (a-log-cell)
                (printf "posn=~a, "
                        (log-cell-posn a-log-cell))
                (printf "theta=~a~n" (log-cell-orientation a-log-cell)))
              data)
  (void))

;; (define (print-frequency data attribute-accessor)
;;   (let ([frequencies (make-vector (expt WORLD-SIZE 2))]
;;         [steps (length data)])
;;     (define (aux n)
;;       (when (< n steps)
;;         (let* ([data-piece (attribute-accessor (vector-ref data n))]
;;                [position (vector-find frequency data-piece])
;;           (if (

(define (vector-find data element)
  (let ([size (vector-length data)])
    (define (aux n)
      (if (< n size)
          (if (equal? (vector-ref data n) element)
              n
              (aux (+ n 1)))
          #f))
    (aux 0)))

;;; Run the simulation
(run-simulation SIM-STEPS)
(print-environment)
;;(print-log-data log-data)

;;; Testing
;; initial world

;; agent

;; simulator

;; action execution

;; passing sensory values




;;; New Representation Prototype
;; function converting the number of grid into x and y coordinates of the agent
;; assume a square environment
;; position->coordinates : N x N -> posn
(define (position->coordinates world-size position)
  (make-posn (remainder position world-size)
             (quotient position world-size)))

(test-case
 "Testing position->cordinates"
 ;; World with size 4
 (let* ([world-size 4]
        [world (build-environment world-size)])
   (check-equal? (position->coordinates world-size 10) (make-posn 2 2) "test 1")
   (check-equal? (position->coordinates world-size 4) (make-posn 0 1) "test 2")
   (check-equal? (position->coordinates world-size 3) (make-posn 3 0) "test 3"))
 ;; World with size 10
 (let* ([world-size 10]
        [world (build-environment world-size)])
   (check-equal? (position->coordinates world-size 53) (make-posn 3 5) "test 4")
   (check-equal? (position->coordinates world-size 99) (make-posn 9 9) "test 5")
   (check-equal? (position->coordinates world-size 27) (make-posn 7 2) "test 6")))