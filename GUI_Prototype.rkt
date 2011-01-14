;; This GUI prototype is supposed to do the following:
;; - to show the world
;; - to allow a step by step acting of the agent
;; - allow the user to change settings

;; Create a frame
;; Develop the design for:
;; - the environment (grid)
;; - objects like walls, doors, the agent, etc.
;; Add buttons
;; - start
;; - stop
;; - step
;; - slower -- faster

(define W #f)
(define H #f)
(define WIDTH #f)
(define HEIGHT #f)

;; agent backup information
(define old-x #f)
(define old-y #f)
(let ([posn (position->coordinates WORLD-SIZE (agent-position A))])
  (set! old-x (posn-x posn))
  (set! old-y (posn-y posn)))

;; Useful pen and brush backup utility
(define backup-pen #f)
(define backup-brush #f)
;; A frame
(define frame (new frame% [label ""]))
(define white-pen (make-object pen% "WHITE" 1 'solid))
(define black-pen (make-object pen% "BLACK" 1 'solid))
(define white-brush (make-object brush% "WHITE" 'solid))
(define black-brush (make-object brush% "BLACK" 'solid))
(define red-brush (make-object brush% "RED" 'solid))
(define dc #f)
(define slider (new slider% [label ""] [min-value 0] [max-value 10] [parent frame]))
(define step-button (new button% [label "Start"] [parent frame]))

(define commands (hash #\w wall
                       #\b box
                       #\s wall-socket
                       #\d door))

(define (handle-key key)
  (let ([cmd (hash-ref commands key #f)])
    (unless (and (symbol? key) (symbol=? key 'release))
      (set! current-object-to-insert cmd)
      (printf "object-to-insert: ~a~n" current-object-to-insert))))

(define current-object-to-insert wall)
(define draw? #f)

(define (handle-mouse-event event)
  (let-values ([(width height) (send dc get-size)])
    (define (set-draw! event-type)
      (cond [(symbol=? event-type 'left-down) (set! draw? #t)]
            [(symbol=? event-type 'left-up) (set! draw? #f)]
            [else (void)]))
    (let ([event-type (send event get-event-type)]
          [x (send event get-x)]
          [y (send event get-y)])
      (set-draw! event-type)
      ;;(printf "mouse-event = ~a; state = ~a~n" event-type mouse-event)
      (when (and draw? (position-valid? x y))
        (let ([new-x (inexact->exact (floor (/ x (/ width WORLD-SIZE))))]
              [new-y (inexact->exact (floor (/ y (/ height WORLD-SIZE))))]
              [w (/ width WORLD-SIZE)]
              [h (/ height WORLD-SIZE)])
          (printf "(~a; ~a)~n" new-x new-y)
          (place-object! current-object-to-insert
                        (coordinates->position WORLD-SIZE (make-posn new-x new-y))
                        env WORLD-SIZE)
          (let ([position (coordinates->position WORLD-SIZE (make-posn new-x new-y))])
            (local-erase new-x new-y w h)
            (draw-object (& env position) (make-posn new-x new-y) w h)))))))

(define (position-valid? x y)
  (and (>= x 0.0) (>= y 0)
       (< x WIDTH) (< y HEIGHT)))

(define (local-erase x y width height)
  (draw-shapes erase-rectangle-design x y width height))

(define (local-draw shape x y width height)
  (draw-shapes erase-rectangle-design x y width height))

(define my-frame%
  (class frame%
    (define/override (on-subwindow-char receiver event)
      (let* ([key (send event get-key-code)])
        (handle-key key)))
    (super-new)))

(define my-canvas%
  (class canvas%
    (define/override (on-subwindow-event receiver event)
      (handle-mouse-event event))
    (super-new)))

(define (init-frame w h)
  ;; Make a w x h frame
  (set! frame (new my-frame% [label "ValueSim"]
                   [width w]
                   [height (+ h 72)]))
  ;; Make the drawing area
  (define canvas (new my-canvas% [parent frame]))
  ;; Get the canvas's drawing context
  (set! dc (send canvas get-dc))
  ;; Make some pens and brushes
  (send dc set-pen black-pen)
  ;; Add a horizontal panel to the frame, with centering for buttons
  (define panel (new horizontal-panel% [parent frame]
                     [alignment '(center top)]
                     [stretchable-height #f]))
  ;; Buttons
  (define toggle-fn (toggle-button))
  (new button% [parent panel]
       [label "Start"]
       ;; Callback procedure for a button click:
       (callback (lambda (button event)
                   (toggle-fn button))))
  (set! step-button (new button% [parent panel]
                         [label "Step"]
                         ;; Callback procedure for a button click:
                         (callback (lambda (button event)
                                     (step)
                                     (draw-world)))))
  (set! slider (new slider% [parent panel]
                    [label "Delay (ms)"]
                    [min-value 0]
                    [max-value 250]))
  ;; draw frame
  (send frame show #t)
  (send dc set-smoothing 'unsmoothed)
  (sleep/yield 1)
  ;; Set some global variables
  (let-values ([(width height) (send dc get-size)])
    (set! WIDTH width)
    (set! HEIGHT height)
    (set! W (/ width WORLD-SIZE))
    (set! H (/ height WORLD-SIZE))))

(define (restore-pen-brush)
  (send dc set-pen black-pen)
  (send dc set-brush white-brush))

(define (toggle-button)
  (define (sim)
    (step)
    ;;(send dc clear)
    (draw-world)
    (sleep (/ (send slider get-value) 1000))
    (sim))
  (define simulation (thread sim))
  (thread-suspend simulation)
  (let ([switch #f])
    (lambda (button)
      (if switch
          (begin
            (send button set-label "Start")
            (thread-suspend simulation)
            (send step-button enable #t)
;;            (draw-grid)
            (draw-elements)
            (draw-agent A))
          (begin
            (send button set-label "Stop")
            (thread-resume simulation)
            (send step-button enable #f)))
      (set! switch (not switch)))))

;; the size of the frame depends on the size of the world?
;; or make the size of the frame constant and unchangable? -- yes
(define (create-view)
  (init-frame 600 600))

;; some part of the frame is devoted to vizualising the grid world
;; another part to control the simulation
;; yet another part to visualize sensory readings and so on

;;; visualizing the grid
;; take the vector with objects
;; for each vector element
;; check the content and visualize accordingly
;; (vector-length env) -- env is a vector with all the lements i need

;; draw the grid first
;; then overwrite the grid with walls and the agent
;; empty places will be skipped
;; for now, take the whole frame to visualize the grid

;; find out where to place the lines
;; WORLD-SIZE is the variable to check
(define (draw-grid)
  (let-values ([(w h) (send dc get-size)])
    ;;(printf "w=~a, h=~a~n"  w h)
    (for ([i (in-range (/ w WORLD-SIZE) w (/ w WORLD-SIZE))])
         (printf "~a~n" i)
         (send dc draw-line i 0 i h))
    (for ([i (in-range (/ h WORLD-SIZE) h (/ h WORLD-SIZE))])
         (send dc draw-line 0 i w i))))

;;; Inserting Objects
(define (draw-elements)
  (let-values ([(width height) (send dc get-size)])
    (let ([w (/ width WORLD-SIZE)]
          [h (/ height WORLD-SIZE)])
      (for ([i (in-range 0 (sqr WORLD-SIZE))])
           (let ([posn (position->coordinates WORLD-SIZE i)])
             (draw-object (& env i) posn w h)))
      (draw-agent A))))

(define (draw-object object posn w h)
  (cond [(and (hash? (tile-object-on-top object))
              (symbol=? 'wall (hash-ref (tile-object-on-top object) 'name)))
         (draw-rectangle (* w (posn-x posn))
                         (* h (posn-y posn))
                         w h 'black 0 'black)]
        [(and (hash? (tile-object-on-top object))
              (symbol=? 'box (hash-ref (tile-object-on-top object) 'name)))
         (draw-shapes box-design (posn-x posn) (posn-y posn) w h)]
        [(and (hash? (tile-object-on-top object))
              (symbol=? 'door (hash-ref (tile-object-on-top object) 'name)))
         (draw-shapes door-design (posn-x posn) (posn-y posn) w h)] 
        [else (void)]))

(define (draw-shapes shapes x y width height)
  (andmap (lambda (a-shape)
            (transform-shape
             a-shape
             (* width x)
             (* height y)
             width
             height))
          shapes))

(define (draw-agent agent)
  (let([posn (position->coordinates WORLD-SIZE (agent-position agent))]
       [shapes (vector-ref agent-design (agent-orientation agent))])
    (draw-shapes shapes (posn-x posn) (posn-y posn) W H)))

(define agent-design
  (vector
   '((rectangle 0.3 0.4 0.4 0.5 black 1 white) ;; body
     (rectangle 0.2 0.45 0.1 0.15 black 1 black) ;; wheels
     (rectangle 0.2 0.7 0.1 0.15 black 1 black)
     (rectangle 0.7 0.45 0.1 0.15 black 1 black)
     (rectangle 0.7 0.7 0.1 0.15 black 1 black)
     (rectangle 0.45 0.3 0.1 0.3 black 1 red) ;; gripper
     (line 0.45 0.3 0.4 0.25 black 1) ;; fingers
     (line 0.4 0.25 0.4 0.15 black 1)
     (line 0.55 0.3 0.6 0.25 black 1)
     (line 0.6 0.25 0.6 0.15 black 1))
   '((rectangle 0.1 0.3 0.5 0.4 black 1 white) ;; body
     (rectangle 0.15 0.2 0.15 0.1 black 1 black) ;; wheels
     (rectangle 0.4 0.2 0.15 0.1 black 1 black)
     (rectangle 0.15 0.7 0.15 0.1 black 1 black)
     (rectangle 0.4 0.7 0.15 0.1 black 1 black)
     (rectangle 0.4 0.45 0.3 0.1 black 1 red) ;; gripper
     (line 0.7 0.45 0.75 0.4 black 1) ;; fingers
     (line 0.75 0.4 0.85 0.4 black 1)
     (line 0.7 0.55 0.75 0.6 black 1)
     (line 0.75 0.6 0.85 0.6 black 1))
   '((rectangle 0.3 0.1 0.4 0.5 black 1 white) ;; body
     (rectangle 0.2 0.15 0.1 0.15 black 1 black) ;; wheels
     (rectangle 0.2 0.4 0.1 0.15 black 1 black)
     (rectangle 0.7 0.15 0.1 0.15 black 1 black)
     (rectangle 0.7 0.4 0.1 0.15 black 1 black)
     (rectangle 0.45 0.4 0.1 0.3 black 1 red) ;; gripper
     (line 0.45 0.7 0.4 0.75 black 1) ;; fingers
     (line 0.4 0.75 0.4 0.85 black 1)
     (line 0.55 0.7 0.6 0.75 black 1)
     (line 0.6 0.75 0.6 0.85 black 1))
   '((rectangle 0.4 0.3 0.5 0.4 black 1 white) ;; body
     (rectangle 0.45 0.2 0.15 0.1 black 1 black) ;; wheels
     (rectangle 0.7 0.2 0.15 0.1 black 1 black)
     (rectangle 0.45 0.7 0.15 0.1 black 1 black)
     (rectangle 0.7 0.7 0.15 0.1 black 1 black)
     (rectangle 0.3 0.45 0.3 0.1 black 1 red) ;; gripper
     (line 0.3 0.45 0.25 0.4 black 1) ;; fingers
     (line 0.25 0.4 0.15 0.4 black 1)
     (line 0.3 0.55 0.25 0.6 black 1)
     (line 0.25 0.6 0.15 0.6 black 1))))

(define door-design
  '((rectangle 0.1 0.1 0.1 0.9 black 0 black)
     (rectangle 0.1 0.1 0.75 0.1 black 0 black)
     (rectangle 0.85 0.1 0.05 0.9 black 0 black)
     (rectangle 0.19 0.19 0.7 0.85 Gray 0 Gray)
     (rectangle 0.2 0.2 0.63 0.8 Orange 0 Orange)
     (ellipse 0.3 0.45 0.2 0.2 DimGray 0.3 Black)))

(define box-design
  '((rectangle 0.3 0.3 0.4 0.4 Green 0.1 Green)))

(define erase-rectangle-design
  '((rectangle 0.0 0.0 1.0 1.0 Black 1 White)))

(define (draw-rectangle x y width height (pen 'black) (pen-width 1) (brush 'white))
  (send dc set-pen (symbol->string pen) pen-width 'solid)
  (send dc set-brush (symbol->string brush) 'solid)
  (send dc draw-rectangle x y (+ width 1) (+ height 1))
  (restore-pen-brush))

(define (draw-ellipse x y width height (pen 'black) (pen-width 1) (brush 'white))
  (send dc set-pen (symbol->string pen) pen-width 'solid)
  (send dc set-brush (symbol->string brush) 'solid)
  (send dc draw-ellipse x y width height)
  (restore-pen-brush))

(define (draw-line x1 y1 x2 y2 (pen 'black) (pen-width 1))  
  (send dc set-pen (symbol->string pen) pen-width 'solid)
  (send dc draw-line x1 y1 x2 y2)
  (restore-pen-brush))

(define (transform-shape shape x y scale-x scale-y)
  (cond [(symbol=? 'rectangle (first shape))
         (let ([base-x (+ x (* scale-x (second shape)))]
               [base-y (+ y (* scale-y (third shape)))])
           ;; (printf "base-x ~a, base-y ~a, a ~a, b ~a, scale-x ~a, scale-y ~a~n"
           ;;         base-x base-y (* scale-x (fourth shape)) (* scale-y (fifth shape))
           ;;         scale-x scale-y)
           (apply draw-rectangle
                  (append (list base-x base-y
                                (* scale-x (fourth shape))
                                (* scale-y (fifth shape)))
                          (drop shape 5))))]
        [(symbol=? 'ellipse (first shape))
         (let ([base-x (+ x (* scale-x (second shape)))]
               [base-y (+ y (* scale-y (third shape)))])
           (apply draw-ellipse
                  (append (list base-x base-y
                                (* scale-x (fourth shape))
                                (* scale-y (fifth shape)))
                          (drop shape 5))))]
        [else ;; line
;;         (send dc set-smoothing 'smoothed)
         (let ([p1-x (+ x (* scale-x (second shape)))]
               [p1-y (+ y (* scale-y (third shape)))]
               [p2-x (+ x (* scale-x (fourth shape)))]
               [p2-y (+ y (* scale-y (fifth shape)))])
           (apply draw-line
                  ;;(printf "~a~n" (list
                  (append (list p1-x p1-y p2-x p2-y)
                          (drop shape 5))))
;;         (send dc set-smoothing 'unsmoothed)
         ]))

(define (make-gui)
  (create-view)
;;  (draw-grid)
  (draw-elements))

(define (draw-world)
  (let ([posn (position->coordinates WORLD-SIZE (agent-position A))])
    (local-erase old-x old-y W H)
    (set! old-x (posn-x posn))
    (set! old-y (posn-y posn))
    (draw-agent A)
    (when box-moved
      (let ([posn (position->coordinates WORLD-SIZE box-moved)])
        ;;(printf "box moved~n")
        (draw-location (posn-x posn) (posn-y posn))))))

(define (draw-location x y)
  (if (position-valid? x y)
      (let ([position (coordinates->position WORLD-SIZE (make-posn x y))])
        (draw-object (& env position) (make-posn x y) W H))
      (printf "invalid location -- DRAW-LOCATION x=~a, y=~a~n" x y)))

  ;; (send dc clear)
  ;; (draw-grid)
  ;; (draw-elements))

;; (define (send dc clear)
;;   (send dc set-brush white-brush)
;;   (send dc set-pen white-pen)
;;   (send dc draw-rectangle
;;         0
;;         0
;;         (send frame get-width)
;;         (send frame get-height))
;;   (restore-pen-brush))


;;; Add buttons
;; - start
;; - stop
;; - step
;; - slower -- faster

;; connect the buttons to their right functions

;;; Objects Visual Design
(make-gui)