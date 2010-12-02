;;; Libraries
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))

;;; Simulator

;;; Tests
;;(require rackunit)
(require rackunit)

;;; Data Types
;; Core environment represented by an array of numbers
(define N 20)
(define environment
  (build-vector (expt N 2)
                (lambda (n)
                  (if (< (random 10) 1)
                      'obstacle
                      'normal))))

(define region-description (make-hash))
(hash-set! region-description 'obstacle 'O)
(hash-set! region-description 'normal '_)

(define agent-view (make-hash))
(hash-set! agent-view 'N 'A)
(hash-set! agent-view 'S 'V)
(hash-set! agent-view 'E '>)
(hash-set! agent-view 'W '<)

(define (print-environment environment size)
  (define (aux n)
    (when (< n (expt size 2))
      (let* ([type (vector-ref environment n)]
             [print-symbol (if (= (agent-posn tested-agent) n)
                               (hash-ref agent-view (agent-orientation tested-agent))
                               (hash-ref region-description type))])
        (when (zero? (modulo n size))
          (printf "\n"))
;;        (printf "~a " n)
        (printf "~a " print-symbol))
      (aux (+ n 1))))
  (aux 0))

;; make a vector of N x N elements
;; compare Norvig
;; must be able to create a random environment with some regions
;; 0.1 change for the block to be an obstacle

;; Agent
(define-struct agent (body mind posn orientation) #:mutable)
(define-struct body (variables sensors motors physics))
(define-struct mind (perception value-system learning decision-making))
(define-struct posn (x y) #:mutable)
(define-struct orientation theta)

(define simple-agent (agent
                      (body '(energy life)
                            null
                            '(move turn-left turn-right)
                            'square)
                      (mind null
                            null
                            null
                            'random)
                      (valid-random-posn)
                      'N))

(define (valid-random-posn)
  (let ([posn (random (expt N 2))])
    (if (symbol=? (vector-ref environment posn) 'normal)
        posn
        (valid-random-posn))))
      

(define tested-agent simple-agent)

(define orientation->movement (make-hash))
(hash-set! orientation->movement 'N (- N))
(hash-set! orientation->movement 'E +1)
(hash-set! orientation->movement 'S N)
(hash-set! orientation->movement 'W -1)


(define (move)
  (let* ([theta (agent-orientation tested-agent)]
         [current-posn (agent-posn tested-agent)]
         [next-posn (+ current-posn (hash-ref orientation->movement theta))])
    (when (symbol=? (vector-ref environment next-posn) 'normal)
      (set-agent-posn! tested-agent next-posn))))

;; (define (turn-left)
;;   (map car (hash->list orientation->movement))
;;   (set-agent-orientation! tested-agent 