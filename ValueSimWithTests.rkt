;;(load "/Users/petr/Dropbox/Libraries/Racket/utils.rkt")
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
(require racket/date)

;;; Objects
(define-struct tile (color temperature object-on-top traversable?)
  #:mutable #:transparent)

(define-struct color (r g b)
  #:mutable #:transparent)

;; Levers
(define (lever #:color [color #f]
               #:temperature [temperature #f])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))  
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (name . lever))))

;; Buttons
(define (button #:color [color #f]
                #:temperature [temperature #f])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))  
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (name . button))))

;; Rocks
(define (wall #:color [color #f]
              #:temperature [temperature #f])
  (unless color
    (set! color (make-color 0 0 0)))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  ;; (when (empty? movable?)
  ;;   (if (zero? (random 2))
  ;;       (set! movable? #t)
  ;;       (set! movable? #f)))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (name . wall))))

;; Boxes
(define (door #:color [color #f]
              #:temperature [temperature #f]
              #:open? [open? #f])
  (unless color
    (set! color (make-color 0 255 0)))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (open? . ,open?)
               (name . door))))

;; Boxes
(define (box #:color [color #f]
             #:temperature [temperature #f])
  (unless color
    (set! color (make-color 255 255 0)))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (name . box))))

;; Battery packs
(define (wall-socket #:color [color #f]
                     #:temperature [temperature #f]
                     #:movable? [movable? #f])
  (unless color
    (set! color (make-color (random 256)
                            (random 256)
                            (random 256))))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (movable? . ,movable?)
               (name . battery-pack))))

;;; Environment
(define (build-environment world-size)
  (let ([environment #f])
    (define (fill-boundaries)
      (define (aux n)
        (cond [(>= n (expt world-size 2)) environment]
              [(or (zero? (modulo (+ n 1) world-size))
                   (zero? (modulo n world-size))
                   (= (quotient n world-size) 0)
                   (= (quotient n world-size) (- world-size 1)))
               (set-tile-object-on-top! (& environment n)
                                        (wall))
               (aux (+ n 1))]
              [else
               (aux (+ n 1))]))
      (aux 0))
    (set! environment
          (build-vector (expt world-size 2)
                        (lambda (n)
                          (tile (make-color 255 255 255) 15 #f 0))))
    (fill-boundaries)))

;;; Agent
(define-struct agent (position orientation energy life fn) #:mutable #:transparent)
(define-struct posn (x y) #:mutable #:transparent)

(define (place-agent-randomly agent environment world-size)
  (let ([position (random (expt world-size 2))])
    (if (tile-object-on-top (& environment position))
        (place-agent-randomly agent environment world-size)
        (begin
          (make-agent position
                      (random 4)
                      (agent-energy agent)
                      (agent-life agent)
                      (agent-fn agent))))))

;; manually place the agent
(define (place-agent agent environment position orientation)
  (cond [(> position (vector-length environment)) agent]
        [(tile-object-on-top (& environment position)) agent]
        [else
         (make-agent position
                     orientation
                     (agent-energy agent)
                     (agent-life agent)
                     (agent-fn agent))]))

(define temp-object #f)

(define (take-object! position environment world-size)
  (let ([tile (& environment position)])
    (set! temp-object (tile-object-on-top tile))
    (set-tile-object-on-top! tile #f)))

(define (place-object! object position environment world-size)
  (let ([tile (& environment position)]
        [obj #f])
    (cond [(procedure? object) (set! obj (object))]
          [else (set! obj object)])
    (set-tile-object-on-top! tile obj)))

;;; Actions
(define (turn-left! agent environment movements)
  (set-agent-energy! agent (reduce-energy (agent-energy agent) 0.5))
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 3)
                                 4)))

(define (turn-right! agent environment movements)
  (set-agent-energy! agent (reduce-energy (agent-energy agent) 0.5))
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 1)
                                 4)))
(define box-moved #f)
(define door-changed #f)

(define (move! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))]
         [moved? #f]
         [next-box-position #f])
    ;; reduce energy
    (set-agent-energy! agent (reduce-energy (agent-energy agent) 10))
    ;; dealing with boxes
    (when (and object-in-front (symbol=? (hash-ref object-in-front 'name) 'box))
      (set! next-box-position (+ next-position (& movements (agent-orientation agent))))
      (unless (tile-object-on-top (& environment next-box-position))
        (take-object! next-position env WORLD-SIZE)
        (place-object! temp-object next-box-position environment WORLD-SIZE)
        (set! moved? #t)
        (set-agent-position! agent next-position)))
    (unless (or object-in-front moved?)
      (set-agent-position! agent next-position))
    (if moved?
        (set! box-moved next-box-position)
        (set! box-moved #f))
    ;; dealing with doors
    (when (and object-in-front
               (symbol=? (hash-ref object-in-front 'name) 'door)
               (hash-ref object-in-front 'open?)) ;; door open
      (set-agent-position! agent next-position))))

(define (move-back! agent environment movements)
  (let* ([next-position (- (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-back (tile-object-on-top (& environment next-position))]
         [moved? #f]
         [next-box-position #f])
    (when (and object-in-back (symbol=? (hash-ref object-in-back 'name) 'box))
      (set! next-box-position (- next-position (& movements (agent-orientation agent))))
      (unless (tile-object-on-top (& environment next-box-position))
        (take-object! next-position env WORLD-SIZE)
        (place-object! temp-object next-box-position environment WORLD-SIZE)
        (set! moved? #t)
        (set-agent-position! agent next-position)))
    (unless (or object-in-back moved?)
      (set-agent-position! agent next-position))
    (if moved?
        (set! box-moved next-box-position)
        (set! box-moved #f))
    ;; going through open doors
    (when (and object-in-back
               (symbol=? (hash-ref object-in-back 'name) 'door)
               (hash-ref object-in-back 'open?)) ;; door open
      (set-agent-position! agent next-position))))

(define (do-nothing! agent environment movements)
  (set-agent-energy! agent (reduce-energy (agent-energy agent) 0.01))
  (void))

(define (open-door! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))])
    ;; a door in front of the agent
    (set! door-changed #f)
    (when (and object-in-front (symbol=? (hash-ref object-in-front 'name) 'door))
      (set! door-changed next-position)
      (hash-set! object-in-front 'color (make-color 190 190 190))
      (hash-set! object-in-front 'open? #t))))

(define (close-door! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))])
    ;; a door in front of the agent
    (set! door-changed #f)
    (when (and object-in-front (symbol=? (hash-ref object-in-front 'name) 'door))
      (set! door-changed next-position)
      (hash-set! object-in-front 'color (make-color 255 255 0))
      (hash-set! object-in-front 'open? #f))))

(define (reduce-energy current-amount amount)
  (let ([next-energy (- current-amount amount)])
    (if (< next-energy 0)
        0
        next-energy)))

(define actions (vector
                 move! turn-left! turn-right!
                 do-nothing! open-door! close-door!
                 move-back!))

;;; Sensors
(define (compute-surrounding-temperatures agent environment movements)
  (define (compute-temperature pos)
    (let* ([a-tile (& environment pos)]
           [object-on-top (tile-object-on-top a-tile)])
      (if object-on-top
          (hash-ref object-on-top 'temperature)
          (tile-temperature a-tile))))
  (define (surrounding-tiles)
    (let* ([position (agent-position agent)]
           [north-position (+ position (& movements 0))]
           [south-position (+ position (& movements 2))])
      (vector
       ;; upper 3 tiles
       (- north-position 1) north-position (+ north-position 1)
       ;; middle tiles
       (- position 1) position (+ position 1)
       ;; middle tiles
       (- south-position 1) south-position (+ south-position 1))))
  (vector-map compute-temperature (surrounding-tiles)))

(define (compute-vision agent environment movements)
  (let ([world-size (sqrt (vector-length environment))])
    (define (turn-left orientation)
      (remainder (+ orientation 3) 4))
    (define (turn-right orientation)
      (remainder (+ orientation 1) 4))
    (define (position-out-of-bounds posn)
      (or (< (posn-x posn) 0) (>= (posn-x posn) world-size)
          (< (posn-y posn) 0) (>= (posn-y posn) world-size)))    
    (define (compute-color posn)
      (if (position-out-of-bounds posn)
          (make-color 0 0 0)
          (let* ([a-tile (& environment (coordinates->position world-size posn))]
                 [object-on-top (tile-object-on-top a-tile)])
            (if object-on-top
                (hash-ref object-on-top 'color)
                (tile-color a-tile)))))
    (define (visible-tiles)
      ;; return 9 positions that are in front of the agent
      (let* ([position (position->coordinates world-size
                                              (agent-position agent))]
             [orientation (agent-orientation agent)]
             [front (& x-y-movements orientation)]
             [left (& x-y-movements (turn-left orientation))]
             [right (& x-y-movements (turn-right orientation))]
             [start (posn+ position front)])
        (vector
         ;; third row -- 5 elements
         (posn+ start front front left left)
         (posn+ start front front left)
         (posn+ start front front)
         (posn+ start front front right)
         (posn+ start front front right right)
         ;; second row -- 3 elements
         (posn+ start front left)
         (posn+ start front)
         (posn+ start front right)
         ;; first row -- 1 element
         start)))
    (vector-map compute-color (visible-tiles))))

;; produces a vector of values
(define (sense agent environment movements)
  (vector-append
   ;; transmitting energy
   (vector
    (agent-energy agent)
    ;; life
    (agent-life agent))
   ;; temperature
   (compute-surrounding-temperatures agent environment movements)
   ;; vision
   (call-with-values
       (lambda () (vector->values
              (vector-map
               (lambda (color) (vector (color-r color)
                                  (color-g color)
                                  (color-b color)))
               (compute-vision agent environment movements))))
     vector-append)))

;; function converting the number of grid into x and y coordinates of the agent
;; assume a square environment
;; position->coordinates : N x N -> posn
(define (position->coordinates world-size position)
  (make-posn (remainder position world-size)
             (quotient position world-size)))

(define (coordinates->position world-size posn)
  (+ (posn-x posn) (* (posn-y posn) world-size)))

(define x-y-movements (vector (make-posn 0 -1) (make-posn 1 0)
                              (make-posn 0 1) (make-posn -1 0)))

(define (posn+ . pars)
  (define (posn+-acc result a-list)
    (cond [(empty? a-list) result]
          [else
           (let ([first-el (first a-list)])
             (posn+-acc
              (list (+ (first result) (posn-x first-el))
                    (+ (second result) (posn-y first-el)))
              (rest a-list)))]))
  (let ([result (posn+-acc '(0 0) pars)])
    (make-posn (first result) (second result))))

(define (posn* posn1 n)
  (make-posn (* (posn-x posn1) n)
             (* (posn-y posn1) n)))

(define (move-in-x-y posn orientation coordinate-movements)
  (posn+ posn (& coordinate-movements orientation)))

;;; Data logger
(define-struct log (percepts value-system-label decision position orientation)
  #:transparent #:mutable )

(define data #())

(define (log-data! percepts value-system-label decision agent data n)
  (! data n
     (make-log percepts value-system-label decision (agent-position agent)
               (agent-orientation agent))))

(define (print-comma-separated data a-file separator end)
  (if (or (number? data) (procedure? data))
      (fprintf a-file "~a~a" data end)
      (begin
        (let ([length (vector-length data)])
          (define (print-comma-separated-aux i)
            (when (< i length)
              (fprintf a-file "~a" (& data i))
              (if (= i (- length 1))
                  (fprintf a-file "~a" end)
                  (fprintf a-file "~a" separator))
              (print-comma-separated-aux (+ i 1))))
          (print-comma-separated-aux 0)))))

;; data format
;; 0 1 2 3 4 ...
;; p o x y a e l
;; pos orienation x y action energy life
(define (save-log data world-size)
  (let ([file-out #f])
    (set! file-out (data-file-open "/Users/petr/Dropbox/rnd1/Simulator/Data/"))
    (printf "~a~n" file-out)
    (vector-map
     (lambda (log)
       (let ([pos (position->coordinates world-size (log-position log))])
         (print-comma-separated (log-position log) file-out ", " ", ")
         (print-comma-separated (log-orientation log) file-out "," ", ")
         (print-comma-separated (posn-x pos) file-out ", " ", ")
         (print-comma-separated (posn-y pos) file-out ", " ", ")
         (print-comma-separated (vector-member (log-decision log) actions)
                                file-out ", " ", ")
         (print-comma-separated (log-percepts log) file-out ", " ", ")
         (print-comma-separated (log-value-system-label log) file-out ", " "\n")))
     data)
    (data-file-close file-out)))

;;; Saving logs into a file
(define (timestamp)
  (let ([now (current-date)]
        [n (open-output-string)])
    (fprintf n "ValueSim-~a-~a-~a-~a-~a-~a"
             (date-year now)
             (date-month now)
             (date-day now)
             (date-hour now)
             (date-minute now)
             (date-second now))
    (get-output-string n)))

(define (data-file-open dir)
  (open-output-file (string-append dir
                                   (timestamp)
                                   ".txt")
                    #:mode 'text
                    #:exists 'replace))

(define (data-file-close file-out)
  (close-output-port file-out))


;;; Value Systems
;; value->utility : fn -> fn (values)
;; to produce a function that maps values according to utility-fn
(define (value->utility utility-fn)
  (lambda (values)
    (utility-fn values)))

;;; Action selection
;; Some simple decision making functions
(define (random-as agent actions)
  (lambda (percepts)
    (let ([decision (& actions (random (vector-length actions)))])
      (values (vector 0) decision))))


;;; Utility functions
;; fixed set points Konidaris and Barto
;; mapping energy to values
;; keep the values between 0.0 and 1.0
(define (energy->value energy priority)
  (- 1 (expt energy (tan (/ (* priority pi) 2)))))
;; need generic functions whose inputs are values or vectors of values
;; will map parts of the sensory stream into values

;; adding priorities
;; simple mappings

;; reconsider the way how agent makes decisions
;; extend the agent that it has value system, learning and action selection

;;; Run simulator
(define (update-world! decision agent environment movements)
  (decision agent environment movements))

;; TODO: there is no reason that the function of the agent returns two values?
;; the architecture of the agent should be connected by the simulator
;; breaking the agent down into the parts will increase the understanding of
;; the code
(define (agent-live agent percepts)
  ((agent-fn agent) percepts))

(define (run-simulation agent environment movements data steps)
  (let ([percepts #f]
        [value-system-label #f]
        [decision #f])
    (define (run-simulation-aux n)
      (if (< n steps)
          (begin
            ;; sense
            (set! percepts (sense agent environment movements))
            ;; TODO: run the value system on the percepts
            ;; TODO: run the learning on the values and percepts
            ;; TODO: make decision considering all above
            ;; let the agent make decision combining new percepts
            (set!-values (value-system-label decision)
                         (agent-live agent percepts))
            ;;(printf "~a~n" decision)
            ;; log data
            (log-data! percepts value-system-label decision agent data n)
            ;; TODO test that the content of all the vectors is copied
            ;; and not the pointers
            ;; update the world
            (update-world! decision agent environment movements)
            (run-simulation-aux (+ n 1)))
          data))
    (set! data (make-vector steps #f))
    (run-simulation-aux 0)))

(define (step)
  (run-simulation A env movements data 1))

(define (run n)
  (set! data (run-simulation A env movements data n))
  (save-log data WORLD-SIZE)
  (void))

(define (make-movements world-size)
  (list->vector `(,(- world-size) +1 ,world-size -1)))

;;; Instantiate environment and agent
;; let's build an environment
(define WORLD-SIZE 30)
(define env (build-environment WORLD-SIZE))
(define movements (make-movements WORLD-SIZE))
(define A (make-agent 0 0 3000 3000 void))
(set-agent-fn! A (random-as A actions))

(define B (make-agent 0 0 100 100 void))
(set-agent-fn! B (random-as B actions))

(set! A (place-agent-randomly A env WORLD-SIZE))

;; Create a simulator
(define STEPS 10)

;;; Tests
(define (test-all)
  (load "Tests.rkt"))

;; (define (random-as probabilities x)
;;   (when (empty? probabilities)
;;     (error "probabilities should not be empty" random-as))
;;   (let ([xnu (- x (car probabilities))])
;;     (if (< xnu 0)
;;         0
;;         (1+ (random-as (cdr probabilities) xnu)))))

;;; Visualization
(define (print-temperatures environment)
  (define (aux n)
    (when (< n (vector-length environment))
      (let* ([tile (& environment n)]
             [object-on-top (tile-object-on-top tile)]
             [symbol #f])
        (if object-on-top
            (set! symbol (hash-ref object-on-top 'temperature))
            (set! symbol (tile-temperature tile)))
        (when (zero? (modulo n WORLD-SIZE))
          (printf "\n"))
        (printf "~a " symbol))
      (aux (+ n 1))))
  (aux 0)
  (printf "~n"))

(define (print-environment agent environment world-size)
  (define (aux n)
    (when (< n (sqr world-size))
      (let* ([tile (& environment n)]
             [symbol (print-symbol n tile agent)])
        (when (zero? (modulo n WORLD-SIZE))
          (printf "\n"))
        (printf "~a " symbol))
      (aux (+ n 1))))
  (aux 0)
  (printf "~n"))

(define (print-symbol n tile agent)
  (cond [(= n (agent-position agent)) 'A]
        [(false? (tile-object-on-top tile)) '_]
        [else 'X]))

(define (print-world)
  (print-environment A env WORLD-SIZE))

;; Buggy version of the function
;; works only for the unary operator case
(define (vector-apply f v)
  (let ([length (vector-length v)])
    (define (vector-apply-aux result n)
      (if (= n length)
          result
          (vector-apply-aux (f result (& v n)) (+ n 1))))
    (if (zero? length)
        (error "No arguments provided in " vector-apply)
        (vector-apply-aux (& v 0) 1))))


;;(require macro-debugger/expand)
;;(require macro-debugger/stepper-text)

;; (require (planet Inaimathi/postscript:1:0))
;; (ps "test1.ps" (0 0 612 792)
;;     (page (translate 50 50)
;;           (text '(0 . 0) "Hello there")
;;           (stroke (square '(0 . 0) 100))))

;; (require racket/system)
;; (define (ps->pdf)
;;   (begin
;;     (ps "test1.ps" (0 0 0 0 )
;;         (page
;;               (text '(100 . 100) "Hello there" #:font (font "Helvetica" 30))
;;               (stroke (square '(100 . 100) 200))))
;;     (system "ps2pdf test1.ps")
;;     (system "rm test1.ps")))

;; (ps->pdf)
;;; Saving logs into a file
(define f #f)

(read-accept-compiled #t)

;;         [o (open-output-string)])
;;     (set! f (get-output-string o))

(define (save-environment file)
  (let ([file (open-output-file file
                                #:mode 'binary
                                #:exists 'replace)])
    (fprintf file "~s" env)
    (close-output-port file)))

;;(define new-world #f)

;;(define-struct tile (color temperature object-on-top traversable?))

(define (load-environment file)
  (let ([file (open-input-file file #:mode 'text)])
    (define result (read/recursive file))
    (close-input-port file)
    (restore-structure result)))

;; to create a structure from a vector of values
(define (restore-structure description)
  (cond [(and (vector? description) (vector-empty? description)) description]
        [(and (vector? description) (not (symbol? (& description 0))))
         (vector-map restore-structure description)]
        [(and (vector? description) (symbol? (& description 0)))
         (let ([symbol (& description 0)]
               [command #f])
           (cond [(symbol=? symbol 'struct:tile) (set! command make-tile)]
                 [(symbol=? symbol 'struct:color) (set! command make-color)]
                 [else
                  (error "unknown structure found -- RESTORE-STRUCTURE" description)])
           (apply command
                  (vector->list
                   (vector-map
                    restore-structure
                    (vector-drop description 1)))))]
        [(hash? description)
         (let ([c (hash-ref description 'color)])
           (hash-set description
                     'color
                     (apply make-color (vector->list (vector-drop c 1)))))]
        [(number? description) description]
        [else #f]))

(define (environment-eq? e1 e2)
  (cond [(and (vector? e1) (not (vector e2)))
         (printf "e1 vector? --  yes; e2 vector? -- no\n")]
        [(and (not (vector? e1)) (vector e2))
         (printf "e1 vector? --  no; e2 vector? -- yes\n")]
        [(vector? e1) (vector-map environment-eq? e1 e2)]))

(define (load-and-set-environment file)
  (let ([new-env (load-environment file)])
    (printf "~a~n" (vector-length new-env))
    (set! WORLD-SIZE (sqrt (vector-length new-env)))
    (set! movements (make-movements WORLD-SIZE))
    (set! env new-env)))

;;(save-environment "env1.txt")
;;(load-and-set-environment "env1.txt")