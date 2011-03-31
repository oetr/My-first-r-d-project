;; Helper module to allow binary search on vectors
;; Author: Petr Samarin, 29 March 2011
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
  
  (check-true (binary-search test-vector 10)) ;; #t
  (check-true (binary-search test-vector 20)) ;; #t
  (check-true (binary-search test-vector 5312)) ;; #t
  (check-true (binary-search test-vector 110)) ;; #t
  (check-true (binary-search test-vector 230)) ;; #t
  (check-false (binary-search test-vector 12)) ;; #f
  (check-false (binary-search test-vector 321)) ;; #f
  (check-false (binary-search test-vector 5213)) ;; #f
  (check-false (binary-search test-vector 6500001)) ;; #f

;;; Binary Trees: some basic functionality --- not used at the moment
  ;; Implemented straight from the "Introduction to Algorithms, 2nd ed." book
  (define-struct bt-node
    (key left right parent) #:mutable #:transparent)

  ;; to return a pointer to a node if a key exists, return #f otherwise
  (define bt-search
    (lambda (node key)
      (let loop ([node node])
        (cond
         [(empty? node) #f]
         [(= key (bt-node-key node)) #t]
         [(< key (bt-node-key node)) (loop (bt-node-left node))]
         [else (loop (bt-node-right node))]))))

  (define bt-minimum
    (lambda (node)
      (let loop ([node node])
        (let ([left (bt-node-left node)])
          (if (empty? left)
              (bt-node-key node)
              (loop left))))))

  (define bt-inorder-walk
    (lambda (node)
      (unless (empty? node)
        (bt-inorder-walk (bt-node-left node))
        (printf "~a~n" (bt-node-key node))
        (bt-inorder-walk (bt-node-right node)))))

  ;; Tree insertion
  (define bt-insert
    (lambda (root-node key)
      ;; find the node where to insert
      (cond
       [(empty? root-node) (make-bt-node key empty empty empty)]
       [(empty? (bt-node-key root-node)) (set-bt-node-key! root-node key)]
       [else
        (let loop ([node root-node] [previous-node empty])
          (cond
           [(empty? node)
            (if (< key (bt-node-key previous-node))
                (set-bt-node-left! previous-node
                                   (make-bt-node key empty empty previous-node))
                (set-bt-node-right! previous-node
                                    (make-bt-node key empty empty previous-node)))]
           [(< key (bt-node-key node)) (loop (bt-node-left node) node)]
           [else (loop (bt-node-right node) node)]))])))

  ;; TODO : Add tree deletion (not necessary for now)

;;; Testing my binary tree implementation
  ;; (define *bt-root-node* (make-bt-node 10 empty empty empty))

  ;; (bt-insert *bt-root-node* 1)

  ;; (bt-search *bt-root-node* 15)

  ;; (bt-minimum *bt-root-node*)

  ;; (bt-inorder-walk *bt-root-node*)

  ;; (map (lambda (key)
  ;;        (bt-insert *bt-root-node* key))
  ;;      '(1 3 0 15 14))
  )
;; eof
