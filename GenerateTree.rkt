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
    (read-useful-chars (read-char port))))

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

;; the user has to make sure not to call the function with arguments out of
;; range of the csv file
(define get-nth-strs
  (lambda (str . n)
    (let ([started #f]
          [counter 0] ;; counts the currently processed number
          [results (make-vector (length n) #f)]
          [finished-counter 0] ;; counts the number of retrieved strings
          [temporary-result '()]
          [fetch-n (sort n <)])
      (call-with-current-continuation
       (lambda (exit)
         (for ([char (in-string str)])
              (cond
               [(symbol=? (char-type char) 'digit)
                (set! temporary-result (cons char temporary-result))]
               [(symbol=? (char-type char) 'comma)
                (when (= (first fetch-n) counter)
                  (vector-set! results finished-counter temporary-result)
                  (set! fetch-n (rest fetch-n))
                  (set! finished-counter (add1 finished-counter))
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
         ;; TODO : check for the wrong parameters before running over the dataset
         (error "parameters are out of range in the csv file -- GET-NTH-STRS" n))))))

(define char-type
  (lambda (c)
    (cond
     [(eof-object? c) 'eof]     
     [(or (char=? c #\.) (char-numeric? c)) 'digit]
     [(char=? c #\,) 'comma]
     [else 'other])))

(define get-nth-csv
  (lambda (port n)
    ;;(when (<= n 0) (error "n should be greated than 0 -- READ-NTH-CSV" n))
    (let loop ([c (get-next-csv port)]
               [n (- n 1)])
      (if (<= n 0)
          (string->number (list->string c))
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
(define dataset-file "../Data/ValueSim-2011-3-28-23-54-22.txt")
;;(define dataset-file "../Data/ValueSim-2011-3-20-12-54-41.txt")

;; find the number of lines in a dataset
(define port #f)
(define DATASET-SIZE 0)
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
(close-input-port port)

(define dataset-actions (make-vector DATASET-SIZE #f))
(define dataset-utilities (make-vector DATASET-SIZE #f))

;; this method does not work in weird cases (e.g., the utility is equal to 2, even
;; though that in the dataset the utility is 0.2something!)
;; TODO : find out later why! 
;; (time* 10
;;  (begin
;;    (printf "~nExtracting actions and utilities from the data set...~n")
;;    (set! port (open-input-file dataset-file #:mode 'text))
;;    ;; read the some of the important values into a list
;;    ;; action is the 5th value
;;    ;; utility is the 43rd value in every line
;;    (for ([i (in-range 0 DATASET-SIZE)])
;;         (vector-set! dataset-actions i (get-nth-csv port 5))
;;         (vector-set! dataset-utilities i (get-nth-csv port 43))
;;         (read-till-line-break port))
;;    (close-input-port port)
;;    (printf "Done~n")))

;; TODO : a faster method, where reading the line is done by using in-built function
(define read-values
  (lambda (port)
    (for ([a-line (in-lines port)]
          [i (in-range 0 DATASET-SIZE)])
         (let ([results (get-nth-strs a-line 4 47)])
           (vector-set! dataset-actions i (vector-ref results 0))
           (vector-set! dataset-utilities i (vector-ref results 1))))))

(time*
 (begin
   (printf "~nExtracting actions and utilities from the data set...~n")
   (define dataset-actions (make-vector DATASET-SIZE #f))
   (define dataset-utilities (make-vector DATASET-SIZE #f))
   (set! port (open-input-file dataset-file #:mode 'text))
   (read-values port)
   (close-input-port port)
   (printf "Done~n")))

;; make sure that all the utilities are in the interval [0; 1] (inclusive)
(for ([utility (in-vector dataset-utilities)])
     (when (or (> utility 1) (< utility 0))
       (error "utility should be in the interval [0; 1] (inclusive), but instead is" utility)))

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

(define maximum-tree-depth 5)

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
(init-vars)
;; Garbage-collect the environment
(clear)
(printf "~n")
(printf "Creating the tree and GC the environment...~n")
(for ([i (in-range 0 DATASET-SIZE)])
     (add-action (& dataset-actions i) i))

;; Saving the tree in a file
;;(generate-and-print-dataset dot-file)
;; Generating the pdf of the graph
;;(generate-pdf dot-file)
;;(system "open dot-tree.gv.pdf")


;;; Get all action sequences that have high and low utilities
;; include the futures library, in order to make use of the parallellism
(require racket/future)
(define THRESHOLD 0.001)

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
     [(not (memq required-index (node-positions a-node))) empty]
     ;; 3) The absolute value of utility of a-node minus initial-utility is not
     ;;    exceeding the THRESHOLD -- continue searching in the children
     [(< (abs (- (vector-ref dataset-utilities required-index)
                 initial-utility))
         threshold)
      (for ([child-node
             (in-vector (vector-filter-not false? (node-children a-node)))])
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
      (for ([child-node
             (in-vector
              (vector-filter-not false? (node-children a-node)))])
           (traverse-tree child-node initial-utility initial-index
                          (+ required-index 1) threshold))])))

;; Do the tree traversal for all of the actions in the start nodes
(define find-interesting-sequences
  (lambda (a-root-node threshold)
    (for* ([start-node
            (in-vector (vector-filter-not false? (node-children a-root-node)))]
           [start-position (in-list (node-positions start-node))]
           [a-node (in-vector (vector-filter-not false? (node-children start-node)))])
          (traverse-tree a-node
                         (vector-ref dataset-utilities start-position)
                         start-position
                         (+ start-position 1)
                         threshold))))

;; To print out information about an action sequence from start to end
;; In addition, the position in the dataset as well as the utilities of
;; all the actions are printed as well
(define print-sequence
  (lambda (start end)
    (printf "~ninitial utility: ~a~n" (vector-ref dataset-utilities start))
    (for ([position (in-range start end)])
         (let ([action (vector-ref dataset-actions position)])
           (printf "~a: ~a -> ~a \t (~a)~n"
                   position
                   action
                   (vector-ref dataset-utilities (+ position 1))
                   (vector-ref ACTION-MEANINGS action))))))

(set! THRESHOLD 0.3)
(set! *results* '())
(find-interesting-sequences *root-node* THRESHOLD)
(printf "~nFound ~a results exceeding the threshold of ~a~n"
        (length *results*)
        THRESHOLD)

(define sorted-list
  (sort 
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
  (lambda (results)
    (sort results (lambda (a-list another-list)
                    (> (caddr a-list) (caddr another-list))))))

|#

;;(print-sequence 1 2)
;;; TODO : Benchmarks to test the performance
;; what is faster: reading all values stored in a list or in a vector?