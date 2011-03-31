;;; Different Value Systems
#|
1. Write a procedure to create concave and convex procedures
2. Apply the procedure onto the current state (as defined by the sensor readings) to get the value of the state
3. That's it?
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

;; Some utility functions
(define utility-of-energy (make-value-function 0 100 0 1 1/10))
(define utility-of-temperature (make-gaussian 25 7))
(define utility-of-proximity (make-value-function 0 5 0 1 5/10))

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

;; Weights global-weight, local weights
(define energy-weights (list 0.5 1.0))
(define temperature-weights (list 0.4 1.0 1.0 1.0 1.0 2.0 1.0 1.0 1.0 1.0))
(define proximity-weights (list 0.1 1.0 1.0 1.0 1.0))

(define weights
  (flatten
   (map (lambda (a-list) (apply make-weights a-list))
        (list energy-weights temperature-weights proximity-weights))))

;; compute the values; use a different value function for each attribute
;; how are the attributes spread out? energy - 1; temperature - 9; proximity - 4
(define attribute-lengths '(1 9 4))

(define compute-values
  (lambda (sensory-readings)
    (let ([current-fn attribute-lengths])
      (for/list ([element sensory-readings]
                 [i (in-range 0 (apply + attribute-lengths))]
                 [weight my-weights])
                (cond [(< i 1) (* weight (utility-of-energy element))]
                      [(< i 10) (* weight (utility-of-temperature element))]
                      [else (* weight (utility-of-proximity element))])))))

;; (apply + (compute-values '(50 10 10 10 10 10 10 10 10 10 1 1 1 1)))
;; apply the weights to the value
