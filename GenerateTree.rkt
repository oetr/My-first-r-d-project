;;; Opens a data set and read some specified values from it
(require racket/system) ;; needed for executing system commands

;;; Open and read the data set
;; reading comma separated values
;; assume the values of importance are all numeric
(define get-next-csv
  (lambda (port)
    (define (read-useful-chars c)
      (if (symbol=? (char-type c) 'digit)
          (cons
           c
           (if (symbol=? (char-type (peek-char port)) 'digit)
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
     ;;[(char-alphabetic? c) 'letter]
     [(or (char=? #\. c) (char-numeric? c)) 'digit]
     [else 'other])))

(define get-nth-csv
  (lambda (port n)
    ;;(when (<= n 0) (error "n should be greated than 0 -- READ-NTH-CSV" n))
    (let loop ([c (get-next-csv port)]
               [n (- n 1)])
      (if (<= n 0)
          c
          (loop (get-next-csv port) (- n 1))))))

;; TODO : an attempt to make the reading faster---right now it takes 13 seconds to
;; read a file with 100_000 lines
;; (define get-nth-number
;;   (lambda (lst n)
;;     (let ([empty-spaces 0])
;;       (for ([i (in-list lst)])
;;            (when (char=? i #\,)
;;              (if (= n empty-spaces)
;;              (set! empty-spaces (+ empty-spaces 1)))

(define read-till-line-break
  (lambda (port)
    (let loop ([c (read-char port)])
      (cond
       [(eq? (char-type c) 'eof) #f]
       [(char=? c #\newline) #t]
       [else (loop (read-char port))]))))

;; open some test data set
(define dataset-file "../Data/ValueSim-2011-3-23-15-32-8.txt")

;; find the number of lines in a dataset
(define port #f)
(define DATASET-SIZE 0)
(time*
 (begin
   (printf "~n")
   (printf "Reading the number of lines in the dataset...~n")
   (set! port (open-input-file dataset-file #:mode 'text))
   (define port-numberof-lines
     (lambda (port)
       (let ([length 0])
         (for ([c (in-lines port)])
              (set! length (+ length 1)))
         length)))
   (set! DATASET-SIZE (port-numberof-lines port))
   (printf "The length of the data set: ~a~n" DATASET-SIZE)
   (close-input-port port)))

(define dataset-actions (make-vector DATASET-SIZE #f))
(define dataset-utilities (make-vector DATASET-SIZE #f))
(time*
 (begin
   (printf "~nExtracting actions and utilities from the data set...~n")
   (set! port (open-input-file dataset-file #:mode 'text))
   ;; read the some of the important values into a list
   ;; action is the 5th value
   ;; utility is the 43rd value in every line
   (for ([i (in-range 0 DATASET-SIZE)])
        (vector-set! dataset-actions i (get-nth-csv port 5))
        (vector-set! dataset-utilities i (get-nth-csv port 43))
        (read-till-line-break port))
   (close-input-port port)
   (printf "Done~n")))

;; TODO : a faster method, where reading the line is done by using in-build function

;; make a tree from the data set
(define ACTION-MEANINGS
  (vector "move" "turn-left" "turn-right"
          "open-door" "close-door" "charge-battery"))
;;(vector-map number->string #(0 1 2 3 4 5)))

(define NUMBER-OF-CHILDREN 6)

(define action->meaning
  (lambda (action)
    (let loop ([i 0])
      (if (string=? action (vector-ref ACTION-MEANINGS i))
          i
          (loop (+ i 1))))))

;;; Tree data structure
(define-struct node (action positions children depth) #:mutable #:transparent)

;; returns the node associated with the action
(define children-ref
  (lambda (node action)
    (vector-ref (node-children node) action)))

(define add-child
  (lambda (node action new-child-node)
    (vector-set! (node-children node) action new-child-node)))

(define make-children
  (lambda ()
    (make-vector NUMBER-OF-CHILDREN #f)))

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
                                  (list position)
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
(define (print-tree->graphviz root-node number-of-actions max-sequence-length port)
  ;; Action counters show what actions have been executed in each depth level
  (let ([node-labels (make-matrix number-of-actions max-sequence-length
                                  (lambda () (make-label 0 '())))])
    (define construct-action-string
      (lambda (action depth-level positions)
        (let ([a-label (matrix-ref node-labels action depth-level)])
          (let ([counter (label-counter a-label)]
                [occurrences (label-occurrences a-label)])
            (let ([node-string (format "~a_~a_~a" action depth-level counter)])
              (set-label-counter! a-label (+ counter 1))
              (set-label-occurrences! a-label (cons positions occurrences))
              node-string)))))
    (define generate-nodes
      (lambda (node depth parent-string)
        (let ([node-string (construct-action-string
                            (node-action node) depth (node-positions node))])
          ;; graphviz uses the token "->" to denote the connection between two nodes
          (fprintf port "~s -> ~s;\n" parent-string node-string)
          ;; loop over all its children
          (let ([children (node-children node)])
            (vector-map
             (lambda (child)
               (when (and child (< (+ depth 1) max-sequence-length))
                 (generate-nodes child (+ depth 1) node-string)))
             children)))))
    ;; print graphviz header
    (fprintf
     port
     "digraph sdsu {
	size=\"36,36\";
        concentrate=true;
	node [color=grey, style=filled, height=.1];
	node [fontname=\"Verdana\", size=\"30,30\"];
	graph [fontname = \"Arial\",
          ranksep=0.3,
	  fontsize = 20,
          style = \"bold\",
          label = \"\\nVisualization of the Tree\\n\",
	  ssize = \"30,60\" ];\n")
    ;; Traverse the tree and print all the nodes
    ;; At the same time, save the 
    (vector-map
     (lambda (child)
       (when child
         (generate-nodes child 0 "start")))
     (node-children root-node))
    ;; reverse all the lists in the action-counters matrix
    (matrix-map
     (lambda (label)
       (set-label-occurrences! label
                               (map reverse (reverse (label-occurrences label)))))
     node-labels)
    ;; Traverse the generated matrix and generate the correct labels for each node
    (for* ([i (in-range number-of-actions)]
           [j (in-range max-sequence-length)])
          (let ([a-label (matrix-ref node-labels i j)])
            (let ([occurrences
                   (list->vector (label-occurrences a-label))])
              (for ([k (in-range (label-counter a-label))])
                   (let ([action-occurrences (vector-ref occurrences k)])
                     (fprintf port
                              "\"~a_~a_~a\" [label=\"~a (~a)\\n" i j k
                              i
                              (vector-ref ACTION-MEANINGS i))
                     (for ([occurrence (in-list action-occurrences)]
                           [utility (map (lambda (index)
                                           (vector-ref dataset-utilities index))
                                         action-occurrences)])
                          (fprintf port "~a  ->  ~a\\n"
                                   occurrence
                                   (/ (round (* 10000 utility)) 10000.0)))
                     (fprintf port "\" ];\n"))))))
    (fprintf port "}")))

;; (generate-and-print-dataset dot-file)

;; (generate-pdf dot-file)

;; a structure to save the count of an action at a depth level
;; and its occurrence in the data set
(define-struct label (counter occurrences) #:transparent #:mutable)

(define make-matrix
  (lambda (rows columns (fill 0))
    (build-vector rows (lambda (dummmy)
                         (build-vector columns (lambda (dummy)
                                                 (if (procedure? fill)
                                                     (fill)
                                                     fill)))))))

(define matrix-map
  (lambda (proc matrix)
    (vector-map
     (lambda (vec)
       (vector-map
        proc
        vec))
     matrix)))

(define matrix-ref
  (lambda (matrix row column)
    (vector-ref (vector-ref matrix row) column)))

(define matrix-set!
  (lambda (matrix row column new-value)
    (vector-set! (vector-ref matrix row) column new-value)))

;;; Opening and closing files
(define dot-file "dot-tree.gv")

(define maximum-tree-depth 20)

;; Print the tree in a graphviz comformable format and save it in a file
(define generate-and-print-dataset
  (lambda (file)
    (let ([p (open-output-file file
                               #:mode 'text
                               #:exists 'replace)])
      (print-tree->graphviz *root-node* 6 maximum-tree-depth p)
      (close-output-port p))))

;; Generate a pdf of the graph
(define generate-pdf
  (lambda (file)
    (system (string-append "dot -Tpdf -Goverlap=false -O " file))))

;;; Run the functions
(time*
 (begin
   (init-vars)
   (printf "~n")
   (printf "Creating the tree...~n")
   (for ([i (in-range 0 DATASET-SIZE)])
        (add-action (& dataset-actions i) i))))

(time*
 (begin
   (printf "~n")
   (printf "Saving the tree in a file and generating the pdf of the graph...~n")
   (generate-and-print-dataset dot-file)
   (generate-pdf dot-file)))

 (clear)
;;; Get all action sequences that have high and low utilities
(define threshold 0.1)

;; compute how good is each action
;; get only those whose absolute value exceeds a threshold

;; traverse the tree and find the good sequences

;;; TODO Benchmarks
;; what is faster: reading all values stored in a list or in a vector?