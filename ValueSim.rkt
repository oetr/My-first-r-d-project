;;; Simulator
;; Environment
(define (make-environment n)
  (make-vector n 0))

(define world-description (make-environment 10))

;; Agent
(define actions '(forward no-op turn-left turn-right))
(define-structure action (effects))

(define-struct posn (x y))
(define agent-position (make-posn 0 0))

;; Sensors
(define battery-charge ())



        
        
        
;;; Tests
;;(require rackunit)
(require rackunit)

;;; Data Types
;; Core environment represented by an array of numbers

;; Agent
         