;; Create a tree that contains all subsequences from some data set
;; Author: Petr Samarin, 30 March 2011
;;; Opens a data set and read some specified values from it
(require racket/system) ;; needed for executing system commands
(require "BinarySearch.rkt")
;; TODO : speed up the coomputation by using multicore parallelism
;; (require racket/future)

;;; Helper functions for reading the numbers from a dataset
;; Read the port linewise and extract the values of interest from each line
(define read-values
  (lambda (port)
    (let ([counter 0])
      (for ([a-line (in-lines port)])
           (let ([results (get-nth-strs a-line 4 47)])
             (set! dataset-actions (cons (vector-ref results 0) dataset-actions))
             (set! dataset-utilities (cons (vector-ref results 1) dataset-utilities)))
           (set! counter (add1 counter)))
      (printf "The length of the data set: ~a~n" counter)
      (set! DATASET-SIZE counter)
      (set! dataset-actions (list->vector (reverse dataset-actions)))
      (set! dataset-utilities (list->vector (reverse dataset-utilities))))))

;; Get all the substrings in str separated by commas that are of interest
;; Takes an infinite number of parameters (n) that all will be searched for
;; The user has to make sure not to call the function with arguments out of
;; range of the csv file
(define get-nth-strs
  (lambda (str . n)
    (let ([started #f]
          [counter 0] ;; counts the currently processed number
          [results (make-vector (length n) #f)]
          [finished-counter 0] ;; counts the number of retrieved strings
          [temporary-result '()] ;; save the results here (discard if not useful)
          [fetch-n (sort n <)]) ;; sorted list of all positions of interest in str
      (call-with-current-continuation ;; scheme's goto on steroids!
       (lambda (exit)
         (for ([char (in-string str)])
              (cond
               [(symbol=? (char-type char) 'digit) ;; continue gathering chars
                (set! temporary-result (cons char temporary-result))]
               [(symbol=? (char-type char) 'comma) ;; start the next substring
                (when (= (first fetch-n) counter)
                  (vector-set! results finished-counter temporary-result)
                  (set! fetch-n (rest fetch-n))
                  (set! finished-counter (add1 finished-counter))
                  ;; Abort the search when all positions found and return them,
                  ;; but first transform them into numbers
                  (when (empty? fetch-n)
                    (exit  
                     (vector-map (lambda (result)
                                   (string->number (list->string (reverse result))))
                                 results))))
                (set! counter (add1 counter))
                (set! temporary-result '())]))
         (when (= (first fetch-n) counter)
           (vector-set! results finished-counter temporary-result)
           (set! fetch-n (rest fetch-n))
           (set! finished-counter (add1 finished-counter))
           (when (empty? fetch-n)
             ;; this is the case when the requested number is in the end of the line
             ;; we need to convert it manually here
             (exit 
              (vector-map (lambda (result)
                            (string->number (list->string (reverse result))))
                          results))))
         ;; if not all the elements could be retrieved (list n is not empty)
         ;; then it means that the user gave wrong parameters
         ;; TODO : check for the wrong parameters before running over the dataset!
         (error "parameters are out of range in the csv file -- GET-NTH-STRS" n))))))

;; Reading comma separated values
;; Assume the values of importance are all numeric
(define get-nth-str
  (lambda (str n)
    (let ([started #f]
          [counter 0]
          [result '()])
      (call-with-current-continuation
       (lambda (exit)
         (for ([char (in-string str)])
              (cond
               [(symbol=? (char-type char) 'digit)
                (set! result (cons char result))]
               [(symbol=? (char-type char) 'comma)
                (when (= n counter)
                  (exit (string->number (list->string (reverse result)))))
                (set! counter (add1 counter))
                (set! result '())]))
         (when (= n counter)
           (string->number (list->string (reverse result)))))))))

(define char-type
  (lambda (c)
    (cond
     [(eof-object? c) 'eof]     
     [(or (char=? c #\.) (char-numeric? c)) 'digit]
     [(char=? c #\,) 'comma]
     [else 'other])))

;;; Reading from the dataset
;; dataset with a million of lines:
;;(define dataset-file "../data/ValueSim-2011-3-30-10-48-53.txt")

;; dataset with 100000 lines
;;(define dataset-file "../data/ValueSim-2011-3-22-16-4-51.txt")

;; dataset with 20000 lines
;;(define dataset-file "../data/ValueSim-2011-3-28-16-35-31.txt")

;; dataset with 1000 of lines:
;;(define dataset-file "../data/ValueSim-2011-3-28-16-26-10.txt")

;; data set with 15 lines
;;(define dataset-file "../data/ValueSim-2011-3-23-15-29-0.txt")

;; 3000 lines
(define dataset-file "../data/ValueSim-2011-4-20-10-4-33.txt")

;; Find the length of the dataset
(define port #f)
(define DATASET-SIZE 0)
(define dataset-actions '())
(define dataset-utilities '())

(printf "~nExtracting actions and utilities from the data set...~n")
(set! port (open-input-file dataset-file #:mode 'text))
(read-values port)
(close-input-port port)
(printf "Done~n")

;; Make sure that all the utilities are in the interval [0; 1] (inclusive)
(for ([utility (in-vector dataset-utilities)])
     (when (or (> utility 1) (< utility 0))
       (error
        "utility should be in the interval [0; 1] (inclusive), but instead is"
        utility)))

;;; Some variables to generate the tree
;; make a tree from the data set
(define ACTION-MEANINGS
  (vector "move" "turn-left" "turn-right"
          "open-door" "close-door" "charge-battery"))
;;(vector-map number->string #(0 1 2 3 4 5)))

(define NUMBER-OF-CHILDREN 6)

(define numerical-action->meaning
  (lambda (action)
    (vector-ref ACTION-MEANINGS action)))

(define action->meaning
  (lambda (action)
    (let loop ([i 0])
      (if (string=? action (vector-ref ACTION-MEANINGS i))
          i
          (loop (+ i 1))))))

;;; Tree data structure
(define-struct node (action positions children depth) #:mutable)

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

;; FIXME : the graphviz interface does not work at the moment!
;; TODO : make the graphviz interface work with the new implementation
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
                                   (/ (round (* 100000 utility)) 100000.0)))
                     (fprintf port "\" ];\n"))))))
    (fprintf port "}")))

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

(define maximum-tree-depth 100)

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

;;(generate-and-print-dataset dot-file)
;;(generate-pdf dot-file)

;;; Run the functions

;; (generate-and-print-dataset dot-file)
;; (generate-pdf dot-file)
(init-vars)
;; Garbage-collect the environment
(clear)
(printf "~n")
(printf "Creating the tree with maximum depth of ~a ...~n" maximum-tree-depth)
(for ([i (in-range 0 DATASET-SIZE)])
     (add-action (& dataset-actions i) i))
(printf "Done~n")

;; To transform all positions in the tree, which are represented by
;; linked lists, into vectors
;; The way the tree is constructed, the lists are sorted in descending order
;; this procedure reverses the lists and makes lists to vectors
;; This allows us to apply binary search on the vectors and make the search faster
;; by a lot!
(define tree-positions-make-vectors
  (lambda (root-node)
    (set-node-positions!
     root-node (list->vector (reverse (node-positions root-node))))
    (vector-map
     tree-positions-make-vectors
     (vector-filter-not false? (node-children root-node)))
    (void)))

;; Transform the lists that represent positions into vectors
;; this procedure is relatively cost-effective, even for large datasets
(printf "~n")
(printf "Transforming the positions from lists into vectors...~n")
(tree-positions-make-vectors *root-node*)
(printf "Done~n")

;; Saving the tree in a file
;;(generate-and-print-dataset dot-file)
;; Generating the pdf of the graph
;;(generate-pdf dot-file)
;;(system "open dot-tree.gv.pdf")


;;; Get all action sequences that have high and low utilities
(define THRESHOLD #f)

;; traverse the tree and find the good sequences
;; 1) compute how good is each action
;; 2) get only those whose absolute value exceeds a threshold

;; Details:
;; start at the root of the tree
;; for all nodes (that represent actions) emanating from the root do: 
;; - compute the utilities on all the occurrences of the actions, let's call them
;;   start utilities
;; - traverse the tree and check if the numbers along the tree
;;   are consequent to the previous
;; - if yes, check difference between the start utility and the utility of the
;;   current action,
;;   if no, discard the search along the path
;; - capture all of the action sequences in a list
;; - when done, sort the list by grouping the same action sequences together

;; Use this list to store the results
(define *results* '())

(define traverse-tree
  (lambda (a-node initial-utility initial-index required-index threshold)
    ;; Several cases are possible here:
    (cond
     ;; 1) Node is empty --- Abort the search
     [(not a-node) empty]
     ;; 2) Required-index is not the same as any index of a-node -- Abort the search
     [(not (binary-search (node-positions a-node) required-index)) empty]
     ;; 3) The absolute value of utility of a-node minus initial-utility is not
     ;;    exceeding the THRESHOLD -- continue searching in the children
     [(< (abs (- (vector-ref dataset-utilities required-index)
                 initial-utility))
         threshold)
      (for ([child-node (in-vector (node-children a-node))])
           ;; only check the non-empty nodes
           (traverse-tree child-node initial-utility initial-index
                          (+ required-index 1) threshold))]
     ;; 4) The abs utility of a-node minus the initial utility exceeds the THRESHOLD
     ;;    save the required-index and continue the search
     [else
      (set! *results*
            (cons (list initial-index
                        required-index
                        (- (vector-ref dataset-utilities required-index)
                           initial-utility))
                  *results*))
      (for ([child-node (in-vector (node-children a-node))])
           (traverse-tree child-node initial-utility initial-index
                          (+ required-index 1) threshold))])))

;; Do the tree traversal for all of the actions in the start nodes
(define find-interesting-sequences
  (lambda (a-root-node threshold)
    ;; make the function run in parallel
    (let ([filtered-vector (vector-filter-not false? (node-children a-root-node))])
      (for* ([start-node (in-vector filtered-vector)]
             [start-position (in-vector (node-positions start-node))]
             [a-node (in-vector (node-children start-node))])
            (traverse-tree a-node
                           (vector-ref dataset-utilities start-position)
                           start-position
                           (+ start-position 1)
                           threshold)))))

;; To print out information about an action sequence from start to end
;; In addition, the position in the dataset as well as the utilities of
;; all the actions are printed as well
(define print-sequence
  (lambda (start end)
    (let ([start-utility (vector-ref dataset-utilities start)]
          [end-utility 0.0])
      (printf "~nstarted with utility = ~a~n" start-utility)
      (for ([position (in-range start end)])
           (let ([action (vector-ref dataset-actions position)]
                 [current-utility (vector-ref dataset-utilities (+ position 1))])
             (set! end-utility current-utility)
             (printf "~a: ~a -> ~a \t (~a)~n"
                     position
                     action
                     ;; show only 10 numbers after the comma
                     (/ (round (* current-utility 100000000.0)) 100000000.0)
                     (vector-ref ACTION-MEANINGS action))))
      (printf "utility gained: ~a~n" (- end-utility start-utility))
      (printf "sequence length: ~a~n" (- end start)))))

(set! THRESHOLD 0.32)
(set! *results* '())
(printf "~n")
(printf "Searching for action sequences that exceed the threshold ~a...~n"
        THRESHOLD)
(find-interesting-sequences *root-node* THRESHOLD)
(printf "~nFound ~a action sequences~n" (length *results*))
(printf "Done~n")

(define sorted-list
  (sort
   ;; filter out the negative utilities
   (filter (lambda (a-list) (positive? (caddr a-list))) *results*)
   (lambda (a-list another-list) (> (caddr a-list) (caddr another-list)))))

#|
;; printing some of the information
(begin
  (newline)
  (printf "All sequences exceeding the threshold of ~a:~n" THRESHOLD)
  *results*)

(begin
  (newline)
  (printf "All positive sequences exceeding the threshold of ~a, sorted by utility difference:~n" THRESHOLD)
  (sort 
   (filter (lambda (a-list) (positive? (caddr a-list))) *results*)
   (lambda (a-list another-list) (> (caddr a-list) (caddr another-list)))))

(define sort-results
  (lambda (results fn)
    (sort results (lambda (a-list another-list)
                    (fn (caddr a-list) (caddr another-list))))))

|#