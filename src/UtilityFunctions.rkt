;;; Different Value Systems
#|
1. Write a procedure to create concave and convex procedures
2. Apply the procedure onto the current state (as defined by the sensor readings) to get the value of the state
3. That's it? Yep
|#
(define make-value-function
  (lambda (attribute-min attribute-max new-min new-max degree-of-convexity)
    (lambda (attribute)
      (let ([new-value (+ new-min
                          (* (/ (- attribute attribute-min)
                                (- attribute-max attribute-min))
                             (- new-max new-min)))])
        (/ new-value (+ new-value (* degree-of-convexity (- 1 new-value))))))))

(define make-gaussian
  (lambda (mu sigma)
    (lambda (x)
      (exp (* -1/2 (sqr (/ (- x mu) sigma)))))))

(define make-weights
  (lambda (overall-weight . weights)
    (when (empty? weights)
      (error "Empty weights -- MAKE-WEIGHTS: " weights))
    (unless (empty? (filter (curryr <= 0) weights))
      (error "All weights should be > 0 -- MAKE-WEIGHTS: " weights))
    (let ([sum (apply + weights)])
      (map (lambda (weight) (* overall-weight (/ weight sum))) weights))))

(define normalize-weights
  (lambda weights
    (when (empty? weights)
      (error "Empty weights -- NORMALIZE-WEIGHTS: " weights))
    (unless (empty? (filter (curryr <= 0) weights))
      (error "All weights should be > 0 -- NORMALIZE-WEIGHTS: " weights))
    (let ([sum (apply + weights)])
      (map (lambda (weight) (/ weight sum)) weights))))