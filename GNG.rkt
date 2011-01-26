;;; Libraries
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
(require racket/date)

;;; Plotting the progress
(define (plot nodes edges gng-dc)
  (andmap (lambda (node)
            (let ([posn (node-position node)])
              (send gng-dc draw-ellipse
                    (& posn 0)
                    (& posn 1)
                    8 8)))
          nodes)
  (andmap (lambda (edge)
            (let ([posn1 (node-position (edge-node1 edge))]
                  [posn2 (node-position (edge-node2 edge))])
              (let ([x1 (+ 4 (& posn1 0))]
                    [y1 (+ 4 (& posn1 1))]
                    [x2 (+ 4 (& posn2 0))]
                    [y2 (+ 4 (& posn2 1))])
                (send gng-dc draw-line x1 y1 x2 y2))))
          edges))

(define (plot-data-point data gng-dc)
  (send gng-dc set-brush yellow-brush)
  (send gng-dc set-pen yellow-pen)
  (send gng-dc draw-ellipse
        (& data 0)
        (& data 1)
        20 20)
  (send gng-dc set-brush blue-brush)
  (send gng-dc set-pen black-pen))

(define gng-dc #f)
;; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define yellow-pen (make-object pen% "yellow" 2 'solid))
(define blue-pen (make-object pen% "RED" 2 'solid))
(define black-pen (make-object pen% "BLACK" 1 'solid))

(define (f)
  ;; Make a 300 x 300 frame
  (define frame (new frame% [label "Plotting GNG"]
                     [width 700]
                     [height 700]))
  ;; Make the drawing area
  (define canvas (new canvas% [parent frame]))
  ;; Get the canvas's drawing context
  (set! gng-dc (send canvas get-dc))
  (send gng-dc set-pen blue-pen)
  (send gng-dc set-brush blue-brush)
  ;; Show the frame
  (send frame show #t)
  ;; Wait a second to let the window get ready
  (sleep/yield 1))


;;; Units
;; need a data structure to represent the units
;; each unit will have a position in R^n and local error
;; position is a vector of real values
;; error is a scalar value
(define-struct node (position error q-value) #:transparent #:mutable)
(define-struct edge (node1 node2 age) #:transparent #:mutable)

;; the connections between the units are saved in a hash table

(define nodes #f)
(define edges #f)

(define (create-GNG-network dimensionality)
  (set! nodes (build-list 2 (lambda (n)
                              (make-node
                               (build-vector dimensionality
                                             (lambda (n) (random 100)))
                               0
                               (random)))))
  (set! edges (list (make-edge (first nodes) (second nodes) 0))))

;; 3. search through the GNG list
;; find the two nearest nodes and return them in a list
(define (find-nearest nodes data)
  (argmin (lambda (node)
            (euclidean-distance (node-position node) data))
          nodes))

;; 4. Find emanating edges from s1
(define (find-emanating-edges edges node)
  (filter (lambda (edge) (or (equal? (edge-node1 edge) node)
                             (equal? (edge-node2 edge) node)))
          edges))

;; 4. increment the age of an edge
(define (increment-age edge)
  (set-edge-age! edge (+ (edge-age edge) 1)))

;; 5. Update local error
(define (update-local-error! node error)
  (set-node-error! node (+ (node-error node) error)))

;; 6. Find neighbors
;; search the edges and return the the nodes that are not the provided node
(define (find-neighbors node edges)
  (map (lambda (edge)
         (let ([node1 (edge-node1 edge)])
           (if (equal? node1 node)
               (edge-node2 edge)
               node1)))
       edges))

;; 6. Move a node towards the input by a fraction
(define (move-by-a-fraction! node x epsilon)
  (let ([position (node-position node)])
    (set-node-position! node
     (vector-process
      + position (vector-mul (vector-process - x (node-position node)) epsilon)))))

;; 7. set the age of the edge to zero, or create if edge nonexistent
(define (insert-or-update-edge node1 node2 edges))

;; 4. set the age to zero
(define (reset-age! edge)
  (set-edge-age! edge 0))

;; 8. remove edges whose age is larger than a-max
;; returns nodes whose edges were removed
(define (remove-edges! check-edges age-max)
  (define (remove-edges-acc new-edges updated-nodes)
    (cond [(empty? edges)
           (set! edges new-edges)
           updated-nodes]
          [(> (edge-age (first edges)) age-max) ;; age more than a-max
           (let ([node1 (edge-node1 (first edges))]
                 [node2 (edge-node2 (first edges))])
             (unless (member node1 updated-nodes)
               (set! updated-nodes
                     (cons node1 updated-nodes)))
             (unless (member node2 updated-nodes)
               (set! updated-nodes
                     (cons node2 updated-nodes)))
             (remove-edges-acc new-edges updated-nodes (rest edges)))]
          [else ;; everything is fine
           (remove-edges-acc (cons (first edges) new-edges)
                             updated-nodes
                             (rest edges))]))
  (remove-edges-acc empty empty edges))

;; 9. Insert a new unit
(define (insert-new-node alpha)
  (let ([highest-error-node #f]
        [highest-error-neighbor #f]
        [emanating-edges empty]
        [new-node #f]
        [edge1 #f]
        [edge2 #f])
    ;; 9. Find the node with maximum accumulate error
    (set! highest-error-node (argmax (lambda (node) (node-error node)) nodes))
    (set! emanating-edges (find-emanating-edges edges highest-error-node))
    (set! highest-error-neighbor
          (argmax (lambda (node) (node-error node))
                  (find-neighbors highest-error-node emanating-edges)))
    ;; 9. Decrease the error of node and its neighbors
    (set-node-error! highest-error-node
                     (* 0.5 (node-error highest-error-node)))
    (set-node-error! highest-error-neighbor
                     (* 0.5 (node-error highest-error-neighbor)))
    ;; 9. Insert a new node between highest and its neighbor
    (set! new-node
          (node (vector-mul
                 (vector-process
                  + (node-position highest-error-node)
                  (node-position highest-error-neighbor))
                 0.5)
                ;; error value from the just-changed node
                (node-error highest-error-node)
                ;; q-value:: TODO--change that the q value is saved as well
                0))
    (set! nodes (cons new-node nodes))
    ;; Remove the edge between node and neighbor
    (set! edges
          (filter (lambda (edge)
                    (let ([node1 (edge-node1 edge)]
                          [node2 (edge-node2 edge)])
                      (not (or
                            (and (equal? node1 highest-error-node)
                                 (equal? node2 highest-error-neighbor))
                            (and (equal? node1 highest-error-neighbor)
                                 (equal? node2 highest-error-node))))))
                  edges))
    (set! edge1 (make-edge new-node highest-error-node 0))
    (set! edge2 (make-edge new-node highest-error-neighbor 0))
    (set! edges (cons edge1 (cons edge2 edges)))
    ;; Insert edges connecting the new unit and node, the new unit and neighbor
    ))

;; final algorithm
(define (GNG-update data)
  ;; 3. find the nearest
  (let ([epsilon-b 0.2] ;; movement fraction for the nearest
        [epsilon-n 0.0006] ;; movement fraction for the neighbors of nearest
        [age-max 50] ;; delete an edge after its age is greater than age-max
        [global-error-decrease 0.995]
        ;; some variables that will be used eventually
        [nearest #f]
        [second-nearest #f]
        [emanating-edges #f]
        [edge-nearest-second-nearest #f]
        [neighbors empty]
        [nodes-to-check empty]
        [current-iteration 0])
    (set! nearest (find-nearest nodes data))
    (set! second-nearest (find-nearest (remove nearest nodes) data))
    ;; 4. Find all emanating edges from nearest node
    (set! emanating-edges (find-emanating-edges edges nearest))
    ;; 4. Increment the age of all the edges
    (map increment-age emanating-edges)
    ;; 5. Update local error
    (set-node-error! nearest (squared-distance (node-position nearest) data))
    ;; find neighbors
    (set! neighbors (find-neighbors nearest emanating-edges))
    (printf "neighbors ~a~n" neighbors)
    (map (lambda (node) (set-node-error! node
                                         (squared-distance (node-position node) data)))
         neighbors)
    ;; 6. Move a node towards the input by two different fractions
    (move-by-a-fraction! nearest data epsilon-b)
    (map (lambda (node) (move-by-a-fraction! node data epsilon-n)) neighbors)
    ;; 7. set the age of the edge to zero, or create if edge nonexistent
    (set! edge-nearest-second-nearest
          (filter (lambda (edge)
                    (let ([node1 (edge-node1 edge)]
                          [node2 (edge-node2 edge)])
                      (or
                       (and (equal? node1 nearest) (equal? node2 second-nearest))
                       (and (equal? node2 nearest) (equal? node1 second-nearest)))))
                  emanating-edges))
    (if (not (empty? edge-nearest-second-nearest))
        (reset-age! (first edge-nearest-second-nearest))
        (set! edges (cons (edge nearest second-nearest 0) edges)))
    ;; 8. remove edges whose age is larger than a-max
    (set! nodes-to-check (remove-edges! emanating-edges age-max))
    ;; 8. A node has no emanating edges, remove it
    (set! nodes
          (remove* (map (lambda (node) (find-neighbors node edges)) nodes-to-check)
                   nodes))
    ;; 10. Decrease all error variables by multiplying them with a constant d
    (andmap (lambda (node) (set-node-error! node (* (node-error node)
                                                    global-error-decrease)))
            nodes)
    ))

(define (run-GNG n-times data-fn)
  (let ([node-insertion-interval 100]
        [alpha 100]
        [data #f])
    (define (run-GNG-aux n)
      (unless (= n n-times)
        (set! data (data-fn))
        (GNG-update data)
        ;; 9. Insert edge when the node-insertion-interval exceeded
        (when (zero? (remainder n node-insertion-interval))
          (insert-new-node alpha))
        (send gng-dc clear)
        (plot nodes edges gng-dc)
        (plot-data-point data gng-dc)
        (sleep 0.01)
        (run-GNG-aux (+ n 1))))
    (create-GNG-network 2)
    (run-GNG-aux 0)))

(define (data-fn)
  (vector (random 350)
          (random 350)))

;; Squared distance between two vectors
(define (squared-distance x1 x2)
  (reduce-vector + 0 (vector-map sqr (vector-map - x2 x1))))

;; Squared distance between two vectors
(define (euclidean-distance x1 x2)
  (sqrt (reduce-vector + 0 (vector-map sqr (vector-map - x2 x1)))))

;; reduce-vector: fn ? vector -> ?
;; to reduce the vector by subsequently applying the operator "fn"
;; starting in the initial state "start"
(define (reduce-vector fn start vector)
  (let ([length (vector-length vector)])
    (define (reduce-vector-acc result n)
      (if (= n length)
          result
          (reduce-vector-acc (+ result (& vector n)) (+ n 1))))
    (reduce-vector-acc start 0)))

;; dealing with matrices
(define (vector-remove matrix row)
  (vector-append
   (vector-take matrix row)
   (vector-drop matrix (+ row 1))))

(define (remove-column matrix column)
  (vector-map (lambda (vector) (vector-remove vector column)) matrix))

(define (vector-mul vec k)
  (vector-map (lambda (v-n) (* v-n k)) vec))

(define (vector-process fn vec1 vec2)
  (vector-map fn vec1 vec2))

