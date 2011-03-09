#|
Author: Petr Samarin
--
The simulator was created as part of my R&D1 project from 1 Dec. 2010 to 31 May. 2011
--
TODO: Describe what the simulator does.
-- Running the Simulator
To run the simulator, download Racket from http://racket-lang.org/download
Evaluate the code either in DrRacket environment, or by running: "racket -f ValueSim.rkt" 
|#
;; Racket libraries
;;(load "/Users/petr/Dropbox/Libraries/Racket/utils.rkt")
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
(require racket/date)

;; My own files
(load "UtilityFunctions.rkt")

;;; Objects
;; Every piece of ground is a tile
;; each tile can have only one object on top
(define-struct tile (color temperature object-on-top traversable?)
  #:mutable #:transparent)

;; Each object and tile has a color
(define-struct color (r g b)
  #:mutable #:transparent)

;; TODO: why are you using hash tables instead of strutures?
;; Walls
(define (wall #:color [color #f]
              #:temperature [temperature #f])
  (unless color
    (set! color (make-color 0 0 0))) ;; black
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (name . wall))))

;; Doors
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

;; Wall sockets
(define (wall-socket #:color [color #f]
                     #:temperature [temperature #f])
  (unless color
    (set! color (make-color 211 211 211)))
  (unless temperature
    (set! temperature (+ 15 (random 10))))
  (make-hash `((color . ,color)
               (temperature . ,temperature)
               (name . wall-socket))))

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

;;; Environment
;; An environment is represented by a vector of tiles

;; build-environment : N -> vector-of-tiles
;; to create an environment whose boundaries are walls
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
                          (tile (make-color 255 255 255) 15 #f 0)))) ;; white color
    (fill-boundaries)))

;;; Agent
;; TODO : fix the structure of the agent function--what are its inputs/outputs?
;; An agent is a structure; fn - is the agent function that returns a decision,
;; as well as the result of the value system
(define-struct agent (position orientation energy action-selection utility-fn)
  #:mutable #:transparent)

;; The position in 2-D space of the agent is represented by a structure
(define-struct posn (x y) #:mutable #:transparent)

;; To place the agent randomly somewhere in the environment, where there is space
(define (place-agent-randomly agent environment world-size)
  (let ([position (random (expt world-size 2))])
    (if (tile-object-on-top (& environment position))
        (place-agent-randomly agent environment world-size)
        (begin
          (make-agent position
                      (random 4)
                      (agent-energy agent)
                      (agent-action-selection agent)
                      (agent-utility-fn agent))))))

;; Manually place the agent
;; TODO : why does this function return the agent in case where it could not place it?
;; TODO : why is a new agent created in case where it could place an agent onto the
;; given position?
(define (place-agent agent environment position orientation)
  (cond [(> position (vector-length environment)) agent]
        [(tile-object-on-top (& environment position)) agent]
        [else
         (make-agent position
                     orientation
                     (agent-energy agent)
                     (agent-life agent)
                     (agent-fn agent))]))

;; TODO : where is this used and what is its purpose?
(define temp-object #f)

;; TODO : is this function even used?
(define (take-object! position environment world-size)
  (let ([tile (& environment position)])
    (set! temp-object (tile-object-on-top tile))
    (set-tile-object-on-top! tile #f)))

;; TODO : is this function even used?
(define (place-object! object position environment world-size)
  (let ([tile (& environment position)]
        [obj #f])
    (cond [(procedure? object) (set! obj (object))]
          [else (set! obj object)])
    (set-tile-object-on-top! tile obj)))

;; set the temperature of the tile (or if it has an object on top, set its temperature
;; as well)
(define set-temperature!
  (lambda (temperature position environment world-size)
    (let ([tile (& environment position)])
      (let ([object-on-top (tile-object-on-top tile)])
        (set-tile-temperature! tile temperature)
        (when object-on-top
          (hash-set! object-on-top 'temperature temperature))))))

;;; Actions
;; These actions are procedures that the agent can use
;; All actions reduce (or increase) the energy of the agent
;; The reduction and increase are all saved in an array

;; Turn the agent left means changing its orientation
;; the turning is all modulo 4, since there are only 4 possible orientations
(define (turn-left! agent environment movements)
  (reduce-energy! agent 'turn-left!)
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 3) 4)))

(define (turn-right! agent environment movements)
  (reduce-energy! agent 'turn-right!)
  (set-agent-orientation! agent (remainder
                                 (+ (agent-orientation agent) 1) 4)))

;; Save information about whether the box/door was moved/changed or not
;; contains #f if not, the hash representing the object if yes
(define box-moved #f)
(define door-changed #f)


;; TODO : think about adding operators of the classical planning representation
;; Moves the agent forward if there is nothing in front of it
;; If there is only one box in front of the agent, then the box is moved as well
;; If there is an open door, then the agent can move
;; If there is more than 2 boxes in front, or any other object other than an empty
;; tile, then the agent cannot move
(define (move! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))]
         [moved? #f]
         [next-box-position #f])
    ;; reduce energy
    (reduce-energy! agent 'move!)
    ;; dealing with boxes
    (when (and object-in-front (symbol=? (hash-ref object-in-front 'name) 'box))
      (set! next-box-position (+ next-position (& movements
                                                  (agent-orientation agent))))
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

;; Doing nothing reduces the energy nevertheless
(define (do-nothing! agent environment movements)
  (reduce-energy! agent 'do-nothing!))

(define (open-door! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))])
    ;; a door in front of the agent
    (reduce-energy! agent 'open-door!)
    (set! door-changed #f)
    (when (and object-in-front (symbol=? (hash-ref object-in-front 'name) 'door))
      (set! door-changed next-position)
      (hash-set! object-in-front 'color (make-color 200 255 200))
      (hash-set! object-in-front 'open? #t))))

(define (close-door! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))])
    ;; a door in front of the agent
    (reduce-energy! agent 'close-door!)
    (set! door-changed #f)
    (when (and object-in-front (symbol=? (hash-ref object-in-front 'name) 'door))
      (set! door-changed next-position)
      (hash-set! object-in-front 'color (make-color 0 255 0))
      (hash-set! object-in-front 'open? #f))))

;; This action allows the agent to charge its battery, but only if the charger
;; is right in front of the agent
(define (charge-battery! agent environment movements)
  (let* ([next-position (+ (agent-position agent)
                           (& movements (agent-orientation agent)))]
         [object-in-front (tile-object-on-top (& environment next-position))])
    (when (and object-in-front
               (symbol=? (hash-ref object-in-front 'name) 'wall-socket))
      (reduce-energy! agent 'charge-battery!))))

;; Reduce the energy of the agent through movements
(define (reduce-energy! agent fn-name)
  (let ([current-amount (agent-energy agent)])
    (let ([next-energy (- current-amount (hash-ref cost fn-name))])
      (cond [(< next-energy 0)
             (set-agent-energy! A 0)]
            [(< next-energy max-energy)
             (set-agent-energy! A next-energy)]
            [else (set-agent-energy! A max-energy)]))))

;; All the actions that the agent is able to perform
(define actions (vector move! turn-left! turn-right!
                        open-door! close-door! charge-battery!))

;; TODO: hash table that shows how every action changes the energy of the agent
(define cost (hash 'move! 0.05 'turn-left! 0.01 'turn-right! 0.01
                   'open-door! 0.1 'close-door! 0.1
                   'charge-battery! -20
                   'move-back! 0.05 'do-nothing! 0.001))

;;; Sensors
;; TODO : give the agent a dedicated temperature sensor and make it decrease or
;; TODO : increase based on the temperature that surrounds the agent
;; Computes 8 tiles around the agent and the agent's own temperature
(define (compute-surrounding-temperatures agent environment movements)
  (define (compute-temperature pos)
    (let* ([a-tile (& environment pos)]
           [object-on-top (tile-object-on-top a-tile)])
      (if object-on-top
          (hash-ref object-on-top 'temperature)
          (tile-temperature a-tile))))
  (define (surrounding-tiles)
    (let ([position (agent-position agent)])
      (let ([north-position (+ position (& movements 0))]
            [south-position (+ position (& movements 2))])
        (vector
         ;; upper 3 tiles
         (- north-position 1) north-position (+ north-position 1)
         ;; middle tiles
         (- position 1) position (+ position 1)
         ;; middle tiles
         (- south-position 1) south-position (+ south-position 1)))))
  (vector-map compute-temperature (surrounding-tiles)))

;; find if there are obstacles in a straight line
;; returns a number if there was one obstacle in its way, and 0 otherwise
;; Works only to the distance of 4 tiles; returns 5 if the distance is greater than 4
(define (send-sonar-beam start-position direction environment)
  (define (send-sonar-beam-aux current-position n-inspected-tiles)
    (let ([object-on-top (tile-object-on-top (& environment current-position))])
      (cond
       ;; dealing with the doors
       [(and object-on-top (symbol=? (hash-ref object-on-top 'name) 'door))
        (if (hash-ref object-on-top 'open?) ;; door open?
            (send-sonar-beam-aux (+ current-position direction)
                                 (+ 1 n-inspected-tiles))
            n-inspected-tiles)] ;; door closed
       [object-on-top n-inspected-tiles] ;; any other object
       ;; 5 is returned if the sonar doesn't receive a reply
       [(= n-inspected-tiles 4) 5]
       ;; beam goes to next tile
       [else (send-sonar-beam-aux (+ current-position direction) 
                                  (+ 1 n-inspected-tiles))])))
  (send-sonar-beam-aux start-position 0))

;; Send 4 beams to front, right, back and left
;; Return a vector of 4 numbers
(define (sense-proximity agent environment movements)
  (let ([position (agent-position agent)]
        [orientation (agent-orientation agent)])
    ;; find the beam directions based on the agent's current orientation
    (let ([front (& movements orientation)]
          [right (& movements (modulo (+ 1 orientation) 4))]
          [back (& movements (modulo (+ 2 orientation) 4))]
          [left (& movements (modulo (+ 3 orientation) 4))])
      (vector
       (send-sonar-beam (+ position front) front environment)
       (send-sonar-beam (+ position right) right environment)
       (send-sonar-beam (+ position back) back environment)
       (send-sonar-beam (+ position left) left environment)))))

;; Compute what colors do objects/tiles have that are in front of the agent
;; The agent can see the tiles as shown below:
;;  x x x x x
;;    x x x
;;      x
;;      A
;; TODO : make the function return a vector of numbers, and not a vector of vectors of
;; numbers
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
      ;; TODO : make the functino use positions and not x/y coordinates
      (let ([position (position->coordinates world-size
                                             (agent-position agent))]
            [orientation (agent-orientation agent)])
        (let ([front (& x-y-movements orientation)])
          (let ([left (& x-y-movements (turn-left orientation))]
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
             start)))))
    (vector-map compute-color (visible-tiles))))

;; produces a vector of values
(define (sense agent environment movements)
  (vector-append
   ;; transmitting energy
   (vector
    (agent-energy agent))
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
     vector-append)
   (sense-proximity agent environment movements)))

;; Function converting the number of grid into x and y coordinates of the agent
;; Assume a square environment
;; position->coordinates : N x N -> posn
(define (position->coordinates world-size position)
  (make-posn (remainder position world-size)
             (quotient position world-size)))

;; Does the opposite of position->coordinates
(define (coordinates->position world-size posn)
  (+ (posn-x posn)
     (* (posn-y posn)
        world-size)))

;; To sum up the all positions
;; example: (posn+ (posn 1 2) (posn 3 4) ... )
;; accepts an endless number of parameters
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

;; To multiply a posn by a scalar
(define (posn* posn1 n)
  (make-posn (* (posn-x posn1) n)
             (* (posn-y posn1) n)))

;; Move in the Eucledean coordinate system
;; given a starting position
(define (move-in-x-y posn orientation coordinate-movements)
  (posn+ posn (& coordinate-movements orientation)))

;;; Data logger
;; A log is a structure that contains all the necessary information about the agent
;; and the environment at some time
(define-struct log (percepts value-system-label decision position orientation)
  #:transparent #:mutable)

;; data is a vector that will save the time-series of the agent in a run
;; TODO : make the agent save the data on the fly, to reduce the memory usage
(define data #())

;; Take all the sensory data, wrap it into a log data structure and save it in data
(define (log-data! percepts value-system-label decision agent data n)
  (! data n
     (make-log percepts value-system-label decision (agent-position agent)
               (agent-orientation agent))))

;; Print the data into a port
(define (print-comma-separated data a-port separator end)
  (if (or (number? data) (procedure? data))
      (fprintf a-port "~a~a" data end)
      (begin
        (let ([length (vector-length data)])
          (define (print-comma-separated-aux i)
            (when (< i length)
              (fprintf a-port "~a" (& data i))
              (if (= i (- length 1))
                  (fprintf a-port "~a" end)
                  (fprintf a-port "~a" separator))
              (print-comma-separated-aux (+ i 1))))
          (print-comma-separated-aux 0)))))

;; The format of the saved data is as follows
;; 0 1 2 3 4 5 6 7 ...
;; p o x y a e l percepts
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
         (print-comma-separated (- world-size 1 (posn-y pos)) file-out ", " ", ")
         (print-comma-separated (vector-member (log-decision log) actions)
                                file-out ", " ", ")
         (print-comma-separated (log-percepts log) file-out ", " ", ")
         (print-comma-separated (log-value-system-label log) file-out ", " "\n")))
     data)
    (data-file-close file-out)))

;;; Saving logs into a file
;; timestamp : -> string
;; To create a string of 
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

;; TODO : check if the function can be written in a better way
;; for example, don't split directory and filename
;; opens a file port (creates a new file, or overwrites the file if it exists)
(define (data-file-open dir)
  (open-output-file (string-append dir
                                   (timestamp)
                                   ".txt")
                    #:mode 'text
                    #:exists 'replace))

;; Close the file port
(define (data-file-close output-port)
  (close-output-port output-port))

;;; Value Systems
;; value->utility : fn -> fn (values)
;; to produce a function that maps values according to utility-fn
(define (value->utility utility-fn)
  (lambda (values)
    (utility-fn values)))

;; A function to produce a sequence of numbers starting from one number and finishing
;; with another one, incrementing by a specified step
(define range
  (lambda (from to (step 1))
    (let loop ([current from] [result empty])
      (if (>= current to)
          (reverse result)
          (loop (+ current step) (cons current result))))))

;; Some utility functions
(define utility-of-energy (make-value-function 0.0 100.0 0.0 1.0 0.5))
(define utility-of-temperature (make-gaussian 25.0 7.0))
(define utility-of-proximity (make-value-function 0.0 5.0 0.0 1.0 0.5))

;; We represent the value system as a set of lists
;; Here we keep the functions that compute the utility of specific sensors
(define utility-functions
  (list utility-of-energy utility-of-temperature utility-of-proximity))
;; This list tells us where to find the information in the sensory stream
;; for example, the first list contains only one address--the address in the
;; sensory stream, under which we can find the current energy of the agent
(define from-to (list (list 0) (range 1 11) (range 37 41)))
;; local weights tell us the relationship between the utilities of the same sensor type
;; for example, they tell us how much more "important" is the utility of one of the
;; temperature sensory compared to all the others... feel free to use any numbers to
;; express the importance of each sensory subchannel (e.g. the temperature that the agent
;; senses underneath itself is 4.0 times as important as all the other temperature utilities
;; that the agent senses.
(define local-weights '((1.0)
                         (1.0 1.0 1.0 1.0 4.0 1.0 1.0 1.0 1.0 1.0)
                         (1.0 1.0 1.0 1.0)))
;; Global weights define the relation between the attributes. For example, the attribute
;; "energy" is more important than other attributes
(define global-weights '(0.5 0.3 0.2))

;; now we need a procedure to put all the information together!
;; 1) get the corresponding region from the sensory stream for each utility function
;; 2) compute utility according to local weights for each utility function
;; 3) sum them up by using global weights
(define compute-utility
  (lambda (utility-functions from-to global-weights local-weights)
    ;; normalize local and global weights
    (lambda (percepts)
      ;; TODO : make the design flexible by adding selectors and generators for the
      ;; sensory stream
      ;; 1) get the right region of the sensory stream
      ;; 2) compute the utility according to the local weights
      (apply +
             (flatten
              (map (lambda (weights values)
                     (map * weights values))
                   (map (lambda (global local) (apply make-weights global local))
                        global-weights
                        local-weights)
                   (map (lambda (fn from-to)
                          (for/list ([i (in-list from-to)])
                                    (fn (vector-ref percepts i))))
                        utility-functions from-to)))))))

(define a (compute-utility utility-functions from-to global-weights local-weights))

;;; Action selection
;; Some simple decision making functions
(define (random-as agent actions)
  (lambda (percepts)
    (vector-ref actions (random (vector-length actions)))))

;; Generate action selection procedures
(define (make-action-selection agent actions)
  (lambda (percepts)
    (let ([utility (agent-utility-fn percepts)])
      (let ([decision (agent-action-selection-fn percepts utility actions)])
        (values utility decision)))))

;;; Utility functions TODO

;;; Run simulator
;; Updates the environment based on the action (decision) of the agent
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
            ;; energy temperature vision
            ;; TODO: run the value system on the percepts
            ;; TODO: run the learning on the values and percepts
            ;; TODO: make decision considering all above
            ;; the only problem is that of data structure flying there and back again
            ;;(set! value-system-labels ((agent-vs agent) percepts))
            ;; (set! decision (action-selection )
            ;; TODO: or decide to link the agent in the beginning
            ;; TODO: motivation--what if the agent does not learn?
            ;; ANSWER: then return a dummy function back...because learning should
            ;; be just a function with a side-effect...it should not produce any
            ;; output
            ;; let the agent make decision combining new percepts
            (set! value-system-label ((agent-utility-fn agent) percepts))
            (set! decision ((agent-action-selection agent) percepts))
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

;; Run the simulation for just one step
(define (step)
  (run-simulation A env movements data 1))

;; Run the simulation for n steps and save the data into the log
(define (run n)
  (set! data (run-simulation A env movements data n))
  (save-log data WORLD-SIZE)
  (void))

;; Generate the movements vector based on the size of the environment
(define (make-movements world-size)
  `#(,(- world-size) +1 ,world-size -1))

;;; Instantiate environment and agent
;; Let's build the environment
(define WORLD-SIZE 30)
(define env (build-environment WORLD-SIZE))
;; With this vector we know in which tile the agent will end up after one movement
(define movements (make-movements WORLD-SIZE))
;; Same as movements, but in x/y coordinates
(define x-y-movements (vector (make-posn 0 -1) (make-posn 1 0)
                              (make-posn 0 1) (make-posn -1 0)))
(define max-energy 100)

(define A (make-agent 0 0 max-energy void void))
(set-agent-action-selection! A (random-as A actions))
(set-agent-utility-fn! A (compute-utility
                          utility-functions
                          from-to
                          global-weights
                          local-weights))

(set! A (place-agent-randomly A env WORLD-SIZE))

;;; Tests
;; Test all the functions in this file
(define (test-all)
  (load "Tests.rkt"))

;;; Console Visualization
(define (print-environment agent environment world-size)
  (define (aux n)
    (when (< n (sqr world-size))
      (let* ([tile (& environment n)]
             [symbol (console-visualize-object n tile agent)])
        (when (zero? (modulo n WORLD-SIZE))
          (printf "\n"))
        (printf "~a " symbol))
      (aux (+ n 1))))
  (aux 0)
  (printf "~n"))

;; This function assigns strings to the objects on the tile
(define (console-visualize-object n tile agent)
  (if (= n (agent-position agent))
      "A"
      (let ([object-on-top (tile-object-on-top tile)])
        (cond
         [(false? object-on-top) "."]
         [(symbol=? 'wall (hash-ref object-on-top 'name)) "O"]
         [(symbol=? 'door (hash-ref object-on-top 'name)) "#"]
         [(symbol=? 'wall-socket (hash-ref object-on-top 'name)) "@"]
         [else '^]))))

;; prints the environment with the standard global variables (A and env)
(define (print-world)
  (print-environment A env WORLD-SIZE))

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

;;; Saving logs into a file
(define f #f)

;; TODO : why is the mode binary?
;; TODO : The loading procedure reads the file in a text mode, so why binary here?
(define (save-environment file)
  (let ([file (open-output-file file
                                #:mode 'binary
                                #:exists 'replace)])
    (fprintf file "~s" env)
    (close-output-port file)))

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
         (begin
           ;; Racket' reader converts #hash(...) expressions into immutable tables
           ;; This simulator need the objects of the world to be mutable
           (set! description (make-hash (hash->list description)))
           (let ([c (hash-ref description 'color)])
             (hash-set! description
                        'color
                        (apply make-color (vector->list (vector-drop c 1))))))
         description]
        [(number? description) description]
        [else #f]))

(define (load-and-set-environment file)
  (let ([new-env (load-environment file)])
    (set! WORLD-SIZE (sqrt (vector-length new-env)))
    (set! movements (make-movements WORLD-SIZE))
    (set! env new-env)
    (set! A (place-agent-randomly A env WORLD-SIZE))))

(load "GUI_Prototype.rkt")
;;(make-gui)
;;(save-environment "env12.txt")
(load-and-set-environment "env1.txt")
