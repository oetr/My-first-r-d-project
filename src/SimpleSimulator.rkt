;;; Simplified Simulation System

;;; Global simulation control variables
(define future-event-list (make-parameter '()))
(define current-time (make-parameter 0.0))
(define current-event (make-parameter #f))
(define event-loop-exit (make-parameter #f))
(define event-loop-next (make-parameter #f))

;;; Event definition and scheduling
;; Each event has a time the event is to be executed, the function to
;; be executed, and the (evaluated) arguments to the function.
(define-struct event (time function arguments))

;; Add an event to the event list.
(define (schedule event)
  (future-event-list (event-schedule event (future-event-list))))

;; Return a new list of events corresponding to the given event added
;; to the given list of events.
(define (event-schedule event event-list)
  (cond ((null? event-list)
         (list event))
        ((< (event-time event)
            (event-time (car event-list)))
         (cons event event-list))
        (else
         (cons (car event-list)
               (event-schedule event (cdr event-list))))))

;;; Simulation control routines
;; Simulate the delay while work is being done.  Add an event to
;; execute the current continuation to the event list.
(define (wait/work delay)
  (let/cc continue
          ;; Add an event to execute the current continuation
          (schedule (event (+ (current-time) delay) continue '()))
          ;; Return to the main loop
          ((event-loop-next))))

;; (start-simulation) -> any
;; This is the main simulation loop.  As long as there are events to
;; be executed (or until the simulation is explicitly stopped), remove
;; the next event from the event list, advance the clock to the time
;; of the event, and apply the event's functions to its arguments.
(define (start-simulation)
  (let/ec exit
    ;; Save the event loop exit continuation
    (event-loop-exit exit)
    ;; Event loop
    (let loop ()
      ;; Exit if no more events
      (when (null? (future-event-list))
        ((event-loop-exit)))
      (let/cc next
              ;; Save the event loop next continuation
              (event-loop-next next)
              ;; Execute the next event
              (current-event (car (future-event-list)))
              (future-event-list (cdr (future-event-list)))
              (current-time (event-time (current-event)))
              (apply (event-function (current-event))
                     (event-arguments (current-event))))
      (loop))))

;; (stop-simulation) -> any
;; Stop the execution of the current simulation (by jumping to its
;; exit continuation).
(define (stop-simulation)
  ((event-loop-exit)))

;; Random Distributions (to remove external dependencies)

;; (random-flat a b) -> inexact-real?
;;   a : real?
;;   b : real?
;; Returns a random real number from a uniform distribution between a
;; and b.
(define (random-flat a b)
  (+ a (* (random) (- b a))))

;; (random-exponential mu) -> inexact-real?
;;   mu : real?
;; Returns a random real number from an exponential distribution with
;; mean mu.
(define (random-exponential mu)
  (* (- mu) (log (random))))

;; Example Simulation Model

;; (generator n) -> any
;;   n : exact-positive-integer?
;; Process to generate n customers arriving into the system.
(define (generator n)
  (do ((i 0 (+ i 1)))
      ((= i n) (void))
    (wait/work 1)
    (schedule (event (current-time) customer (list i)))))

;; (customer i) -> any
;;   i : exact-nonnegative-integer?
;; The ith customer into the system.  The customer is in the system
;; 2 to 10 minutes and then leaves.
(define (customer i)
  (printf "~a: customer ~a enters~n" (current-time) i)
  (wait/work (random 10))
  (printf "~a: customer ~a leaves~n" (current-time) i))

;; (run-simulation n) -> any
;;   n : exact-positive-integer?
;; Run the simulation for n customers (or until explicitly stopped at
;; some specified time).
(define (run-simulation n)
  ;; Create new global values
  (parameterize ((future-event-list '())
                 (current-time 0.0)
                 (current-event #f)
                 (event-loop-exit #f)
                 (event-loop-next #f))
    ;; Schedule the customer generator
    (schedule (event 0.0 generator (list n)))
    ;; Stop the simulation at the specified time (optional)
                                        ;(schedule (event 50.0 stop-simulation '()))
    ;; Start the simulation main loop
    (start-simulation)))

;; Run the simulation for 10 customers.
(run-simulation 10)

;;; Understanding continuations
(define p1 (make-parameter 1))
(define p2 (make-parameter 2))

(parameterize ([p1 3] [p2 (p1)])
  (cons (p1) (p2)))

(let ([k (let/cc out
           (parameterize ([p1 2])
             (p1 3)
             (cons (let/cc k
                     (out k))
                   (p1))))])
  (if (procedure? k)
      (k (p1))
      k))