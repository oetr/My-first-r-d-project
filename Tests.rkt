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
 "Color of the tiles in range"
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
 "Boundaries should not be movable (7x7)"
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