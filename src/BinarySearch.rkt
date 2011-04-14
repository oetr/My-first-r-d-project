;; Helper module to allow binary search on vectors
;; Author: Petr Samarin

(module binary-search racket/gui
  (require rackunit)
  (provide binary-search)
  
;;; Binary search on vectors
  ;; To binary search a vector
  ;; Returns true if the key is in the vector and false otherwise
  (define binary-search
    (lambda (vec key)
      (let ([length (vector-length vec)])
        (if (zero? length)
            #f
            (let loop ([left 0] [right (- length 1)])
              (let ([center (quotient (+ right left) 2)])
                (cond
                 [(or (< key (vector-ref vec left)) (> key (vector-ref vec right))) #f]
                 [(or (= key (vector-ref vec left)) ;; key found
                      (= key (vector-ref vec center))
                      (= key (vector-ref vec right))) #t]
                 [(< key (vector-ref vec center))
                  (if (= (- center left) 1)
                      #f
                      (loop left center))]
                 [else
                  (if (= (- right center) 1)
                      #f
                      (loop center right))])))))))

  ;; Testing binary search
  (define test-vector (vector 1 2 10 11 20 110 230 431 5312 64361 1436712894))
  
  (check-true (binary-search test-vector 10))
  (check-true (binary-search test-vector 20))
  (check-true (binary-search test-vector 5312))
  (check-true (binary-search test-vector 110))
  (check-true (binary-search test-vector 230))
  (check-false (binary-search test-vector 12))
  (check-false (binary-search test-vector 321))
  (check-false (binary-search test-vector 5213))
  (check-false (binary-search test-vector 6500001)))

  ;; eof
  