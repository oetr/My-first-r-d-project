;;; Opens a data set and read some specified values from it

;; require some libraries
(require racket/system)

;;; Open and read the data set
;; reading comma separated values
;; assume the values of importance are all numeric
(define get-next-csv
  (lambda (port)
    (define (read-useful-chars c)
      (if (memq (char-type c) '(letter digit))    
          (cons
           c
           (if (memq (char-type (peek-char port)) '(letter digit))
               (read-useful-chars (read-char port))
               '()))
          (read-useful-chars (read-char port))))
    (string->number
     (list->string
      (read-useful-chars (read-char port))))))

(define char-type
  (lambda (c)
    (cond
     [(eof-object? c) 'eof]
     [(char-alphabetic? c) 'letter]
     [(or (char=? #\. c) (char-numeric? c)) 'digit]
     [else 'other])))

(define get-nth-csv
  (lambda (port n)
    (when (<= n 0) (error "n should be greated than 0 -- READ-NTH-CSV" n))
    (let loop ([c (get-next-csv port)]
               [n (- n 1)])
      (if (<= n 0)
          c
          (loop (get-next-csv port) (- n 1))))))

(define read-till-line-break
  (lambda (port)
    (let loop ([c (read-char port)])
      (cond
       [(eq? (char-type c) 'eof) #f]
       [(char=? c #\newline) #t]
       [else (loop (read-char port))]))))

;; open some test data set
(define file "../Data/ValueSim-2011-3-7-13-49-52.txt")
;;(define file "../Data/ValueSim-2011-3-7-13-0-53.txt")

;; TODO : find the number of lines in a dataset

(define port (open-input-file file #:mode 'text))
(define DATASET-SIZE 2000)
(define dataset-actions (make-vector DATASET-SIZE #f))
(define dataset-utilities (make-vector DATASET-SIZE #f))
;; read the some of the important values into a list
;; action is the 5th value
;; utility is the 47th value in every line
(for ([i (in-range 0 DATASET-SIZE)])
     (vector-set! dataset-actions i (get-nth-csv port 5))
     (vector-set! dataset-utilities i (get-nth-csv port 42))
     (read-till-line-break port))
(close-input-port port)


(vector-length dataset-utilities)

;; another method, where reading the line is done by using in-build function
(define a (for/vector ([line (in-lines port)])
                      line))

(read (& a 1))



;; make a tree from the data set
;;; Tree data structure
(define-struct node (action positions children depth) #:mutable #:transparent)

(define children-ref
  (lambda (node action)
    (hash-ref (node-children node) action #f)))

(define add-child
  (lambda (node action child-node)
    (hash-set! (node-children node) action child-node)))

(define make-children (lambda () (make-hash)))

(define make-position (lambda () '()))

(define add-position
  (lambda (node new-position)
    (set-node-positions! node (cons new-position (node-positions node)))))

(define *root-node* (make-node '() '() (make-children) 0))

;; The fringe keeps track of ongoing action sequences
(define *fringe* (list *root-node*))

;;; Creting the tree
(define tree
  (lambda (node action position)
    (let ([child-node (children-ref node action)])
      (if child-node
          (begin  ;; a child node associated with that action already exists
            (add-position child-node position)
            child-node)
          (begin ;; a node does not exist: child-node = #f
            (let ([depth (node-depth node)])
              (unless (>= depth maximum-tree-depth)
                (let ([new-node
                       (make-node action
                                  '(1)
                                  (make-children)
                                  (+ 1 (node-depth node)))])
                  (add-child node action new-node)
                  new-node))))))))

;; Appends the action to every sequence on the fringe
(define add-action
  (lambda (action position)
    (set! *fringe*
          (cons *root-node*
                (filter (lambda (a) (not (void? a)))
                        (map (lambda (node) (tree node action position))
                             *fringe*))))))

;; Initialize global variables *root-node* and *fringe*
(define init-vars
  (lambda ()
    (set! *root-node* (make-node '() '() (make-children) 0))
    (set! *fringe* (list *root-node*))))

;; Write the tree in graphviz format into a port
(define (print-tree root-node number-of-actions max-sequence-length port)
  (let ([action-counters (make-matrix number-of-actions max-sequence-length)]
        [action-meanings (vector "0" "1" "2" "3" "4" "5" "6")])
    (define construct-action-string
      (lambda (action depth-level)
        (let ([action-counter (matrix-ref action-counters action depth-level)])
          (let ([node-string (format "~a_~a_~a" action depth-level action-counter)])
            (matrix-set! action-counters action depth-level (+ action-counter 1))
            node-string))))
    (define loop
      (lambda (node depth parent-string)
        (let ([node-string (construct-action-string
                            (node-action node) depth)])
          ;; print node
          (fprintf port "~s -> ~s;\n" parent-string node-string)
          ;; loop over all its wchildren
          (let ([children (hash-values (node-children node))])
            (unless (empty? children)
              (andmap
               (lambda (child)
                 (when (< (+ depth 1) max-sequence-length)
                   (loop child (+ depth 1) node-string)))
               (hash-values (node-children node))))))))
    (fprintf
     port
     "digraph sdsu {
	size=\"36,36\";
        concentrate=true;
	node [color=grey, shape=ellipse, style=filled, height=.1];
	node [fontname=\"Verdana\", size=\"30,30\"];
	graph [fontname = \"Arial\",
          ranksep=20,
	  fontsize = 36,
          style = \"bold\",
          label = \"\\nDataset Visualization\\n\",
	  ssize = \"30,60\" ];\n")
    (andmap (lambda (child) (loop child 0 "start"))
            (hash-values (node-children root-node)))
    (for* ([i (in-range number-of-actions)]
           [j (in-range max-sequence-length)])
          (for ([k (in-range (matrix-ref action-counters i j))])
               (fprintf port "\"~a_~a_~a\" [label=~s];\n" i j k
                        (vector-ref action-meanings i))))
    (fprintf port "}")))

(define make-matrix
  (lambda (rows columns (fill 0))
    (build-vector
     rows
     (lambda (dummmy)
       (build-vector columns
                     (lambda (dummy) fill))))))

(define matrix-ref
  (lambda (matrix row column)
    (vector-ref (vector-ref matrix row) column)))

(define matrix-set!
  (lambda (matrix row column new-value)
    (vector-set! (vector-ref matrix row) column new-value)))

;;; Open/close a file
(define file "dot-tree.gv")
(define (data-file-open file)
  (open-output-file file
                    #:mode 'text
                    #:exists 'replace))
(define (data-file-close file-out)
  (close-output-port file-out))

(define maximum-tree-depth 6)

;; Write the tree into a file
(define generate-and-print-dataset
  (lambda (file)
    (let ([p (data-file-open file)])
      (print-tree *root-node* 6 maximum-tree-depth p)
      (data-file-close p))))

;;; Run the functions
(init-vars)

(for ([i (in-range 0 DATASET-SIZE)])
     (add-action (& dataset-actions i) i))

(generate-and-print-dataset file)

(system "circo -Tpdf -Goverlap=false -O dot-tree.gv")

(clear)