;;; Libraries
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
 (let* ([test-name "Moving around 5x5 "]
        [world-size 5]
        [environment (build-environment world-size)]
        [an-agent (make-agent 0 0 0 0)]
        [movements #f])
   (set! movements (list->vector `(,(- world-size) +1 ,world-size -1)))
   (set! an-agent (place-agent an-agent environment 13 3))
   ;; Move horizontally
   (move! an-agent environment movements)
   (check-equal? 12 (agent-position an-agent) (string-append test-name ":test " (number->string 1)))
   (check-equal? 3 (agent-orientation an-agent) (string-append test-name ":test " (number->string 1)))
   ;; Move
   (move! an-agent environment movements)
   (check-equal? 11 (agent-position an-agent) (string-append test-name ":test " (number->string 2)))
   (check-equal? 3 (agent-orientation an-agent) (string-append test-name ":test " (number->string 2)))
   ;; Move
   (move! an-agent environment movements)
   (check-equal? 11 (agent-position an-agent) (string-append test-name ":test " (number->string 3)))
   (check-equal? 3 (agent-orientation an-agent) (string-append test-name ":test " (number->string 3)))
   ;; Move vertically
   (set! an-agent (place-agent an-agent environment 18 0))
   ;; Move
   (move! an-agent environment movements)
   (check-equal? 13 (agent-position an-agent) (string-append test-name ":test " (number->string 4)))
   (check-equal? 0 (agent-orientation an-agent) (string-append test-name ":test " (number->string 4)))
   ;; Move
   (move! an-agent environment movements)
   (check-equal? 8 (agent-position an-agent) (string-append test-name ":test " (number->string 5)))
   (check-equal? 0 (agent-orientation an-agent) (string-append test-name ":test " (number->string 5)))
   ;; Move
   (move! an-agent environment movements)
   (check-equal? 8 (agent-position an-agent) (string-append test-name ":test " (number->string 6)))
   (check-equal? 0 (agent-orientation an-agent) (string-append test-name ":test " (number->string 6)))
   ))

(test-case
 "Vision"
 ;; World with size 5
 (let* ([test-name #f]
        [world-size 41]
        [environment (build-environment world-size)]
        [an-agent (make-agent 0 0 0 0)]
        [movements #f]
        [vision #f])
   ;; Posittion: x=39, y=39, facing the wall int the east
   (set! test-name "Posittion: x=39, y=39, facing the wall int the east")
   (set! movements (list->vector `(,(- world-size) +1 ,world-size -1)))
   (set! an-agent (place-agent an-agent environment
                               (coordinates->position world-size (make-posn 39 39)) 1))
   ;;   (printf "~a~n" an-agent)
   ;; Vision(4)
   (set! vision (compute-vision an-agent environment movements))
   (check-equal? (make-color 0 0 0)
                 (& vision 4) (string-append test-name ":test " (number->string 0)))
   ;; Vision(5)
   (set! vision (compute-vision an-agent environment movements))
   (check-equal? (make-color 0 0 0)
                 (& vision 5) (string-append test-name ":test " (number->string 1)))
   ;; Vision(6)
   (set! vision (compute-vision an-agent environment movements))
   (check-equal? (make-color 0 0 0)
                 (& vision 6) (string-append test-name ":test " (number->string 2)))
   ;; Vision(7)
   (set! vision (compute-vision an-agent environment movements))
   (check-equal? (make-color 0 0 0)
                 (& vision 7) (string-append test-name ":test " (number->string 3)))
   ;; Vision(8)
   (set! vision (compute-vision an-agent environment movements))
   (check-equal? (make-color 0 0 0)
                 (& vision 8) (string-append test-name ":test " (number->string 4)))
   ;; Posittion: x=1, y=39, facing the wall in the east
   (set! test-name "Posittion: x=1, y=39, facing the wall int the west")
   (set! an-agent (place-agent an-agent environment
                               (coordinates->position world-size (make-posn 1 39)) 3))
   ;; Vision(0)
   (set! vision (compute-vision an-agent environment movements))   
   (check-equal? (make-color 0 0 0)
                 (& vision 0) (string-append test-name ":test " (number->string 1)))
   ;; Vision(5)
   (check-equal? (make-color 0 0 0)
                 (& vision 5) (string-append test-name ":test " (number->string 2)))
   ;; Vision(6)
   (check-equal? (make-color 0 0 0)
                 (& vision 6) (string-append test-name ":test " (number->string 3)))
   ;; Vision(7)
   (check-equal? (make-color 0 0 0)
                 (& vision 7) (string-append test-name ":test " (number->string 4)))
   ;; Vision(8)
   (check-equal? (make-color 0 0 0)
                 (& vision 8) (string-append test-name ":test " (number->string 5)))
   ;; Vision(2)
   (set! vision (compute-vision an-agent environment movements))   
   (check-not-equal? (make-color 0 0 0)
                     (& vision 2) (string-append test-name ":test " "6"))
   
   ))

(test-case
 "Testing position->cordinates"
 ;; World with size 4
 (let* ([test-name  "Testing position->cordinates (4x4)"]
        [world-size 4]
        [world (build-environment world-size)])
   (check-equal? (position->coordinates world-size 10) (make-posn 2 2)
                 (string-append test-name "test: 1"))
   (check-equal? (position->coordinates world-size 4) (make-posn 0 1)
                 (string-append test-name "test: 2"))
   (check-equal? (position->coordinates world-size 3) (make-posn 3 0)
                 (string-append test-name "test: 3"))
   (check-equal? (position->coordinates world-size 3) (make-posn 3 0)
                 (string-append test-name "test: 3.5")))
 ;; World with size 10
 (let* ([test-name "Testing position->cordinates (10x10) "]
        [world-size 10]
        [world (build-environment world-size)])
   (check-equal? (position->coordinates world-size 53) (make-posn 3 5)
                 (string-append test-name "test: 4"))
   (check-equal? (position->coordinates world-size 99) (make-posn 9 9)
                 (string-append test-name "test: 5"))
   (check-equal? (position->coordinates world-size 27) (make-posn 7 2)
                 (string-append test-name "test: 6"))
   (check-equal? (position->coordinates world-size 0) (make-posn 0 0)
                 (string-append test-name "test: 7"))))

(test-case
 "Testing coordinates->position "
 ;; World with size 4
 (let* ([test-name "Testing coordinates->position (4x4) "]
        [world-size 4]
        [world (build-environment world-size)])
   (check-equal? (coordinates->position world-size (make-posn 2 2)) 10
                 (string-append test-name "test: 1"))
   (check-equal? (coordinates->position world-size (make-posn 0 1)) 4
                 (string-append test-name "test: 2"))
   (check-equal? (coordinates->position world-size (make-posn 3 0)) 3
                 (string-append test-name "test: 3"))
   (check-equal? (coordinates->position world-size (make-posn 3 0)) 3
                 (string-append test-name "test: 4")))
 ;; World with size 10
 (let* ([test-name "Testing coordinates->position (10x10) "]
        [world-size 10]
        [world (build-environment world-size)])
   (check-equal? (coordinates->position world-size (make-posn 3 5)) 53
                 (string-append test-name "test: 1"))
   (check-equal? (coordinates->position world-size (make-posn 9 9)) 99
                 (string-append test-name "test: 2"))
   (check-equal? (coordinates->position world-size (make-posn 7 2)) 27
                 (string-append test-name "test: 3"))
   (check-equal? (coordinates->position world-size (make-posn 7 2)) 27
                 (string-append test-name "test: 4"))
   (check-equal? (coordinates->position world-size (make-posn 0 0)) 0
                 (string-append test-name "test: 5"))))






