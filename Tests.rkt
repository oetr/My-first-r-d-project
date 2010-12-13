(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
(require rackunit)

(test-case
 "Filling the world(4x4) with tiles"
 (let* ([name "Filling the world(4x4) with tiles"]
        [world-size 4]
        [world (build-environment world-size)])
   (for ([i (in-range (expt world-size 2))])
        (check-true (tile? (vector-ref world i)))))
 "Filling the world(10x10) with tiles"
 (let* ([test-name "Filling the world(10x10) with tiles"]
        [world-size 10]
        [world (build-environment world-size)])
   (for ([i (in-range (expt world-size 2))])
        (check-true (tile? (vector-ref world i))
                    (string-append test-name ", test: "
                                   (number->string i))))))

(test-case
 "Color of the ground tiles is close to green"
 (let* ([test-name  "Color of the tiles in range "]
        [world-size 10]
        [world (build-environment world-size)])
   (for ([i (in-range (expt world-size 2))])
        (check-true (< (color-r (tile-color (vector-ref world i)))
                       20) (string-append "R " test-name
                                          (number->string i)))
        (check-true (> (color-g (tile-color (vector-ref world i)))
                       200) (string-append "G " test-name
                                           (number->string i)))
        (check-true (< (color-b (tile-color (vector-ref world i)))
                       20) (string-append "B " test-name
                                          (number->string i))))))

(test-case
 "Boundaries should not be movable"
 (let* ([test-name "Boundaries should not be movable (3x3) "]
        [world-size 3]
        [world (build-environment world-size)])
   (check-false (hash-ref (tile-object-on-top (vector-ref world 0))
                          'movable?) (string-append test-name ", test: 1"))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 1))
                          'movable?) (string-append test-name ", test: 2"))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 2))
                          'movable?) (string-append test-name ", test: 3"))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 3))
                          'movable?) (string-append test-name ", test: 4")))
 (let* ([test-name "Boundaries should not be movable (7x7) "]
        [world-size 7]
        [world (build-environment world-size)])
   (check-true (hash? (tile-object-on-top (vector-ref world 0))))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 0))
                          'movable?) (string-append test-name ", test: 1"))
   (check-true (hash? (tile-object-on-top (vector-ref world 6))))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 6))
                          'movable?) (string-append test-name ", test: 2"))
   (check-true (hash? (tile-object-on-top (vector-ref world 48))))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 48))
                          'movable?) (string-append test-name ", test: 3"))
   (check-true (hash? (tile-object-on-top (vector-ref world 41))))
   (check-false (hash-ref (tile-object-on-top (vector-ref world 41))
                          'movable?) (string-append test-name ", test: 4"))))

(test-case
 "Random placement: Agent is not placed on occupied space "
 (let* ([test-name "Random placement: Agent is not placed on occupied space "]
        [world-size 4]
        [environment (build-environment world-size)]
        [agent (make-agent 0 0 0 0)])
   (for ([i (in-range 20)])
        (set! agent (place-agent-randomly agent environment world-size))
        ;; (printf "agent: ~a~n" agent)
        ;; (print-environment agent environment world-size)
        (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                     (string-append test-name (number->string i))))))


(test-case
 "Manual placement: Agent is not placed on occupied space "
 (let* ([test-name "Manual placement: Agent is not placed on occupied space "]
        [world-size 5]
        [environment (build-environment world-size)]
        [agent (make-agent 0 0 0 0)])
   ;; Placing the agent at 7
   (set! agent (place-agent agent environment 7 0))
   (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                (string-append test-name ": test " (number->string 1)))
   (check-equal? 7 (agent-position agent) (string-append test-name ": test " (number->string 1)))                                                         
   ;; Placing the agent at 8
   (set! agent (place-agent agent environment 8 1))
   (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                (string-append test-name ":test " (number->string 2)))
   (check-equal? 8 (agent-position agent) (string-append test-name ":test " (number->string 2)))
   ;; Placing the agent at 9
   (set! agent (place-agent agent environment 9 2))
   (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                (string-append test-name ":test " (number->string 3)))
   (check-equal? 8 (agent-position agent) (string-append test-name ":test " (number->string 3)))
   ;; Placing the agent at 10
   (set! agent (place-agent agent environment 10 3))
   (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                (string-append test-name ":test " (number->string 4)))
   (check-equal? 8 (agent-position agent) (string-append test-name ":test " (number->string 4)))
   ;; Placing the agent at 20
   (set! agent (place-agent agent environment 20 0))
   (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                (string-append test-name ":test " (number->string 5)))
   (check-equal? 8 (agent-position agent) (string-append test-name ":test " (number->string 5)))
   ;; Placing the agent at 17
   (set! agent (place-agent agent environment 17 1))
   (check-false (tile-object-on-top (vector-ref environment (agent-position agent)))
                (string-append test-name ":test " (number->string 6)))
   (check-equal? 17 (agent-position agent) (string-append test-name ":test " (number->string 6)))))

(test-case
 "Agent turns left"
 ;; World with size 4
 (let* ([test-name "Agent turns left "]
        [world-size 4]
        [environment (build-environment world-size)]
        [agent (make-agent 0 0 0 0)])
   (set! agent (place-agent agent environment 6 0))
   ;; Turn left
   (turn-left! agent environment)
   (check-equal? (agent-position agent) 6 (string-append test-name ":test " (number->string 1)))
   (check-equal? (agent-orientation agent) 3 (string-append test-name ":test " (number->string 1)))
   ;; Another turn left
   (turn-left! agent environment)
   (check-equal? 6 (agent-position agent) (string-append test-name ":test " (number->string 2)))
   (check-equal? 2 (agent-orientation agent) (string-append test-name ":test " (number->string 2)))
   ;; Another turn left
   (turn-left! agent environment)
   (check-equal? 6 (agent-position agent) (string-append test-name ":test " (number->string 3)))
   (check-equal? 1 (agent-orientation agent) (string-append test-name ":test " (number->string 3)))
   ;; Another turn left
   (turn-left! agent environment)
   (check-equal? 6 (agent-position agent) (string-append test-name ":test " (number->string 4)))
   (check-equal? 0 (agent-orientation agent) (string-append test-name ":test " (number->string 4)))
   ;; Another turn left
   (turn-left! agent environment)
   (check-equal? 6 (agent-position agent) (string-append test-name ":test " (number->string 5)))
   (check-equal? 3 (agent-orientation agent) (string-append test-name ":test " (number->string 5)))   
   ))

(test-case
 "Agent turns right"
 ;; World with size 5
 (let* ([test-name "Agent turns right "]
        [world-size 5]
        [environment (build-environment world-size)]
        [agent (make-agent 0 0 0 0)])
   (set! agent (place-agent agent environment 13 0))
   ;; Turn right
   (turn-right! agent environment)
   (check-equal? 13 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 1 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ;; Another turn right
   (turn-right! agent environment)
   (check-equal? 13 (agent-position agent) (string-append test-name ":test " (number->string 2)))
   (check-equal? 2 (agent-orientation agent) (string-append test-name ":test " (number->string 2)))
   ;; Another turn right
   (turn-right! agent environment)
   (check-equal? 13 (agent-position agent) (string-append test-name ":test " (number->string 3)))
   (check-equal? 3 (agent-orientation agent) (string-append test-name ":test " (number->string 3)))
   ;; Another turn right
   (turn-right! agent environment)
   (check-equal? 13 (agent-position agent) (string-append test-name ":test " (number->string 4)))
   (check-equal? 0 (agent-orientation agent) (string-append test-name ":test " (number->string 4)))
   ;; Turn right
   (turn-right! agent environment)
   (check-equal? 13 (agent-position agent) (string-append test-name ":test " (number->string 5)))
   (check-equal? 1 (agent-orientation agent) (string-append test-name ":test " (number->string 5)))   
   ))

(test-case
 "Moving around"
 ;; World with size 5
 (let* ([test-name "Moving around "]
        [world-size 5]
        [environment (build-environment world-size)]
        [agent (make-agent 0 0 0 0)])
   (set! agent (place-agent agent environment 13 3))
   ;; Move
   (move! agent environment)
   (check-equal? 12 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 3 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ;; Move
   (move! agent environment)
   (check-equal? 11 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 3 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ;; Move
   (move! agent environment)
   (check-equal? 11 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 3 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ;; Setting the agent to another position
   (set! agent (place-agent agent environment 18 0))
   ;; Move
   (move! agent environment)
   (check-equal? 13 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 0 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ;; Move
   (move! agent environment)
   (check-equal? 8 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 0 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ;; Move
   (move! agent environment)
   (check-equal? 8 (agent-position agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 0 (agent-orientation agent) (string-append test-name ":test " (number->string 1)))
   ))


(test-case
 "Testing position->cordinates"
 ;; World with size 4
 (let* ([world-size 4]
        [world (build-environment world-size)])
   (check-equal? (position->coordinates world-size 10) (make-posn 2 2) "test 1")
   (check-equal? (position->coordinates world-size 4) (make-posn 0 1) "test 2")
   (check-equal? (position->coordinates world-size 3) (make-posn 3 0) "test 3")
   (check-equal? (position->coordinates world-size 3) (make-posn 3 0) "test 3.5"))
 ;; World with size 10
 (let* ([world-size 10]
        [world (build-environment world-size)])
   (check-equal? (position->coordinates world-size 53) (make-posn 3 5) "test 4")
   (check-equal? (position->coordinates world-size 99) (make-posn 9 9) "test 5")
   (check-equal? (position->coordinates world-size 27) (make-posn 7 2) "test 6")
   (check-equal? (position->coordinates world-size 0) (make-posn 0 0) "test 6")))