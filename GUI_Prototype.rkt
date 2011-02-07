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

(define WIDTH #f) ;; width of the canvas
(define HEIGHT #f)
(define W #f) ;; width of a tile to draw on the canvas
(define H #f)

;; This procedure is called when initiating the canvas
;; or changing the number of tiles
(define (set-global-variables)
  (let-values ([(width height) (send dc get-size)])
    (set! WIDTH width)
    (set! HEIGHT height)
    (set! W (/ width WORLD-SIZE))
    (set! H (/ height WORLD-SIZE)))
  (let ([posn (position->coordinates WORLD-SIZE (agent-position A))])
    (set! old-x (posn-x posn))
    (set! old-y (posn-y posn))))

;; Agent's backup information about its old position
(define old-x #f)
(define old-y #f)

;; Used for backing up the pens and brushes during the painting
(define backup-pen #f)
(define backup-brush #f)
;; Main frame
(define frame #f)
(define dc #f) ;; main drawing context
(define slider #f) ;; slider to change the simulation speed
(define step-button #f) ;; button to step
;; Sensor canvas' drawing contexts
(define vision-dc #f)
(define temperature-dc #f)
(define proximity-dc #f)
(define energy-dc #f)
;; pens and brushes
(define white-pen (make-object pen% "WHITE" 1 'solid))
(define black-pen (make-object pen% "BLACK" 1 'solid))
(define white-brush (make-object brush% "WHITE" 'solid))
(define black-brush (make-object brush% "BLACK" 'solid))
(define red-brush (make-object brush% "RED" 'solid))
(define energy-color (make-object color% 211 211 211))

;; Handles keyboard keys that define which object is to be inserted
(define (handle-key key)
  (let ([cmd (hash-ref commands key #f)])
    (unless (and (symbol? key) (symbol=? key 'release))
      (set! current-object-to-insert cmd)
      (printf "object-to-insert: ~a~n" current-object-to-insert))))

;; Key mappings for inserting objects
(define commands (hash #\w wall
                       #\b box
                       #\s wall-socket
                       #\d door))

#|
When a mouse is pressed, check whether the coordinates are valid--inside
of the main drawing canvas (dc)
Then keep track of mouse releases and presses--toggles "started-drawing?"

|#
(define (handle-mouse-event event)
  (let-values ([(width height) (send dc get-size)])
    (define (set-draw! event-type)
      (cond [(symbol=? event-type 'left-down) (set! started-drawing? #t)]
            [(symbol=? event-type 'left-up) (set! started-drawing? #f)]
            [else (void)]))
    (let ([event-type (send event get-event-type)]
          [x (send event get-x)]
          [y (send event get-y)])
      ;;(printf "(~a, ~a)~n" x y)
      (set-draw! event-type)
      (when (and started-drawing? (position-valid? x y))
        ;; find the tile corresponding to the mouse' coordinates
        (let ([new-x (inexact->exact (floor (/ x (/ width WORLD-SIZE))))]
              [new-y (inexact->exact (floor (/ y (/ height WORLD-SIZE))))])
          ;; using "place-object!" procedure  provided by the simulator model
          (place-object! current-object-to-insert
                         (coordinates->position WORLD-SIZE (make-posn new-x new-y))
                         env WORLD-SIZE)
          (let ([position (coordinates->position WORLD-SIZE (make-posn new-x new-y))])
            (local-erase new-x new-y W H)
            (draw-object (& env position) (make-posn new-x new-y) W H)))))))

;; Need to know what to insert
(define current-object-to-insert wall)
;; Should we draw or not?--the mouse being pressed and released triggers drawing
(define started-drawing? #f)
;; Are the x and y inside of the main canvas defined by (WIDTH; HEIGHT)
(define (position-valid? x y)
  (and (>= x 0.0) (>= y 0)
       (< x WIDTH) (< y HEIGHT)))

;; To erase a specific rectangle given its coordinates and the overall size of the frame
(define (local-erase x y width height)
  (draw-shapes erase-rectangle-design x y width height))

;; To draw a shape in specific position
(define (local-draw shape x y width height)
  (draw-shapes erase-rectangle-design x y width height))

;; Making my own frame to be able to handle keyboard events
;; TODO: how about overriding the "on-paint" method
(define my-frame%
  (class frame%
    (define/override (on-subwindow-char receiver event)
      (let* ([key (send event get-key-code)])
        (handle-key key)))
    (super-new)))

;; Making my own canvas in order to support drawing with the mouse
(define my-canvas%
  (class canvas%
    (define/override (on-subwindow-event receiver event)
      (handle-mouse-event event))
    (super-new)))

(define (init-frame w h)
  (define marg 4)
  ;; Make a w x h frame
  (set! frame (new my-frame%
                   [label "ValueSim"]
                   [width w]
                   [height (+ h 72)] ;; + 72 to make the canvas have proper height
                   [style '(metal no-resize-border)])) 
  (define main-panel (new horizontal-panel%
                          [parent frame]
                          [alignment '(center center)]))
  (define world-panel (new vertical-panel%
                           [parent main-panel]
                           [alignment '(left center)]
                           [min-width 600]
                           [stretchable-width #f]))
  ;; Make the drawing area
  (define canvas (new my-canvas%
                      [parent world-panel]
                      [min-width 600]
                      [min-height 600]
                      [stretchable-width #f]
                      [stretchable-height #f]))
  ;; Get the canvas's drawing context
  (set! dc (send canvas get-dc))
  ;; Make some pens and brushes
  (send dc set-pen black-pen)
  ;; Add a horizontal panel to the frame, with centering for buttons
  (define down-left-panel (new horizontal-panel%
                               [parent world-panel]
                               [alignment '(center top)]
                               [stretchable-height #f]))
  ;; Start/Stop button
  (define toggle-fn (toggle-button))
  (new button% [parent down-left-panel]
       [label "Start"]
       ;; Callback procedure for a button click:
       [callback (lambda (button event)
                   (toggle-fn button))])
  ;; Step button
  (set! step-button (new button%
                         [parent down-left-panel]
                         [label "Step"]
                         ;; Callback procedure for a button click:
                         [callback (lambda (button event) (step) (draw-world))]))
  ;; Slider
  (set! slider (new slider% [parent down-left-panel]
                    [label "Delay (ms)"]
                    [min-value 0]
                    [max-value 250]))
  ;; Sensors panel
  (define right-panel (new vertical-panel%
                           [parent main-panel]
                           [alignment '(left center)]))
  (define sensor-panel (new horizontal-panel%
                            [parent right-panel]
                            [alignment '(left center)]
                            [min-height 100]
                            [min-width 424]
                            [stretchable-height #f]
                            [stretchable-width #f]
                            [spacing 10]))
  (new panel%
       [parent sensor-panel]
       [min-width 5]
       [stretchable-width #f])
  (define down-panel (new horizontal-panel%
                          [parent right-panel]
                          [alignment '(center top)]
                          [vert-margin 100]))
  ;; Vision panel
  (define vision-panel (new vertical-panel%
                            [parent sensor-panel]
                            [min-width 120]
                            [vert-margin marg]
                            [horiz-margin marg]
                            [stretchable-width #f]
                            [alignment '(center top)]))
  (new message%
       [parent vision-panel]
       [label "Vision"])
  ;; canvas for the vision panel
  (define vision-canvas (new canvas%
                             [parent vision-panel]
                             [min-width 120]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]))
  (set! vision-dc (send vision-canvas get-dc))
  (send vision-dc set-pen black-pen)
  ;; Temperature panel
  (define temperature-panel (new vertical-panel%
                                 [parent sensor-panel]
                                 [alignment '(center top)]
                                 [vert-margin marg]
                                 [horiz-margin marg]
                                 [min-width 100]
                                 [stretchable-width #f]))
  (new message% [parent temperature-panel]
       [label "Temperature"])
  ;; canvas for the temperature panel
  (define temperature-canvas (new canvas% [parent temperature-panel]
                                  [min-width 100]
                                  [min-height 100]
                                  [stretchable-width #f]
                                  [stretchable-height #f]))
  (set! temperature-dc (send temperature-canvas get-dc))
  ;; Proximity panel
  (define proximity-panel (new vertical-panel%
                               [parent sensor-panel]
                               [alignment '(center top)]
                               [vert-margin marg]
                               [horiz-margin marg]
                               [min-width 100]
                               [stretchable-width #f]))
  (new message% [parent proximity-panel] [label "Proximity"])
  ;; canvas for the temperature panel
  (define proximity-canvas (new canvas% [parent proximity-panel]
                                  [min-width 100]
                                  [min-height 100]
                                  [stretchable-width #f]
                                  [stretchable-height #f]))
  (set! proximity-dc (send proximity-canvas get-dc))
  (send proximity-dc set-smoothing 'smoothed)
  ;; Energy panel
  (define energy-panel (new vertical-panel%
                            [parent sensor-panel]
                            [alignment '(center top)]
                            [vert-margin marg]
                            [min-width 75]
                            [stretchable-width #f]))
  (new message%
       [parent energy-panel]
       [label "Energy"])
  (define energy-canvas (new canvas%
                             [parent energy-panel]
                             [style '(no-focus)]
                             [min-height 30]
                             [stretchable-height #f]))
  (set! energy-dc (send energy-canvas get-dc))
  ;; draw frame
  (send frame show #t)
  (send dc set-smoothing 'unsmoothed)
  (sleep/yield 1)
  ;; Set some global variables like WIDTH, HEIGHT, W, H
  (set-global-variables))

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
            (thread-suspend simulation)
            (draw-agent A)
            ;;            (draw-elements)
            (send button set-label "Start")
            ;; (draw-grid)
            (send step-button enable #t))
          (begin
            (send button set-label "Stop")
            (thread-resume simulation)
            (send step-button enable #f)))
      (set! switch (not switch)))))

;; the size of the frame depends on the size of the world?
;; or make the size of the frame constant and unchangable? -- yes
(define (create-view)
  (init-frame 1024 600))

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
  (let ([is-a-hash? (hash? (tile-object-on-top object))])
    (cond [(and is-a-hash?
                (symbol=? 'wall (hash-ref (tile-object-on-top object) 'name)))
           (draw-rectangle (* w (posn-x posn))
                           (* h (posn-y posn))
                           w h 'black 0 'black)]
          [(and is-a-hash?
                (symbol=? 'box (hash-ref (tile-object-on-top object) 'name)))
           (draw-shapes box-design (posn-x posn) (posn-y posn) w h)]
          [(and is-a-hash?
                (symbol=? 'door (hash-ref (tile-object-on-top object) 'name)))
           (let ([design #f])
             (if (hash-ref (tile-object-on-top object) 'open?)
                 (set! design (& door-design 1))
                 (set! design (& door-design 0)))
             (draw-shapes design (posn-x posn) (posn-y posn) w h))]
          [(and is-a-hash?
                (symbol=? 'wall-socket (hash-ref (tile-object-on-top object) 'name)))
           (draw-shapes wall-socket-design (posn-x posn) (posn-y posn) w h)]
          [else
           (draw-shapes erase-rectangle-design (posn-x posn) (posn-y posn) w h)])))

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
    (draw-location (posn-x posn) (posn-y posn))
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
  (vector 
   '((rectangle 0.1 0.1 0.1 0.9 black 0 black)
     (rectangle 0.1 0.1 0.75 0.1 black 0 black)
     (rectangle 0.85 0.1 0.05 0.9 black 0 black)
     (rectangle 0.19 0.19 0.7 0.8 Gray 0 Gray)
     (rectangle 0.2 0.2 0.63 0.7 Orange 0 Orange)
     (ellipse 0.3 0.45 0.2 0.2 DimGray 0.3 Black))
   '((line 0.1 0.1 0.1 0.95 Orange 1.5)
     (line 0.1 0.1 0.85 0.1 Orange 1.5)
     (line 0.9 0.1 0.9 0.95 Orange 1.5))))

(define wall-socket-design
  '((rectangle 0.0 0.0 1.0 1.0 black 0 Gray)
    (ellipse 0.2 0.2 0.6 0.6 Black 0 White)
    (ellipse 0.32 0.45 0.1 0.1 Black 0 Black)
    (ellipse 0.58 0.45 0.1 0.1 Black 0 Black)))

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
         (let ([p1-x (+ x (* scale-x (second shape)))]
               [p1-y (+ y (* scale-y (third shape)))]
               [p2-x (+ x (* scale-x (fourth shape)))]
               [p2-y (+ y (* scale-y (fifth shape)))])
           (apply draw-line
                  (append (list p1-x p1-y p2-x p2-y)
                          (drop shape 5))))]))

(define (make-gui)
  (create-view)
  (draw-grid)
  (draw-elements))

(define (draw-world)
  (let ([posn (position->coordinates WORLD-SIZE (agent-position A))]
        [new-x #f]
        [new-y #f])
    (set! new-x (posn-x posn))
    (set! new-y (posn-y posn))    
    (local-erase old-x old-y W H)
    (draw-location old-x old-y)
    (set! old-x new-x)
    (set! old-y new-y)
    ;; drawing a box that has been moved
    (when box-moved
      (let ([new-box-posn (position->coordinates WORLD-SIZE box-moved)])
        ;;(printf "box moved~n")
        (draw-location (posn-x new-box-posn) (posn-y new-box-posn))))
    ;; drawing a door that has been either opened or closed
    (when door-changed
      (let ([new-door-posn (position->coordinates WORLD-SIZE door-changed)])
        (local-erase (posn-x new-door-posn) (posn-y new-door-posn) W H)
        (draw-location (posn-x new-door-posn) (posn-y new-door-posn))
        (set! door-changed #f)))
    (draw-agent A)
    (draw-sensors)))

(define (draw-location x y)
  (if (position-valid? x y)
      (let ([position (coordinates->position WORLD-SIZE (make-posn x y))])
        (draw-object (& env position) (make-posn x y) W H))
      (printf "invalid location -- DRAW-LOCATION x=~a, y=~a~n" x y)))

;;; Testing the individual panels
(define (draw-data-rectangles rows columns x y w h sw sh dc data)
  (let ([colors #f]
        [positions #f])
    (set! colors (map (lambda (col)
                        (make-object color%
                                     (color-r col)
                                     (color-g col)
                                     (color-b col)))
                      (vector->list data)))
    (set! positions
          (for*/list ([i (in-range y (+ y (* (+ h sh) rows)) (+ h sh))]
                      [j (in-range x (+ x (* (+ w sw) columns)) (+ w sw))])
                     (list j i)))
    (andmap (lambda (position color)
              (send dc set-brush color 'solid)
              (send dc draw-rectangle
                    (first position)
                    (second position)
                    w h))
            positions
            colors)))

(define (draw-temperature-text rows columns x y w h sw sh dc data)
  (let ([temperatures #f]
        [positions #f])
    (set! temperatures (vector->list data))
    (set! positions
          (for*/list ([i (in-range y (+ y (* (+ h sh) rows)) (+ h sh))]
                      [j (in-range x (+ x (* (+ w sw) columns)) (+ w sw))])
                     (list j i)))
    (andmap (lambda (position temperature)
              (send dc draw-text
                    (number->string temperature)
                    (first position)
                    (second position)))
            positions
            temperatures)))

(define (draw-energy energy-dc)
  (let-values ([(width height) (send energy-dc get-size)])
    (send energy-dc set-pen "White" 1 'transparent)
    (send energy-dc set-brush "White" 'solid)
    (let-values ([(dc-width dc-height) (send energy-dc get-size)])
      (send energy-dc draw-rectangle 0 0 dc-width dc-height))
    (send energy-dc set-pen "Black" 1 'transparent)
    (send energy-dc set-brush energy-color 'solid)
    (send energy-dc draw-rectangle 0 0 (/ (* (agent-energy A) width) 30000) height)))

(define (draw-temperature temperature-dc)
  (let-values ([(width height) (send temperature-dc get-size)])
    (let ([temperatures (compute-surrounding-temperatures A env movements)]
          [sw 5]
          [sh 5]
          [w1 20]
          [start-x 17]
          [start-y (/ (- height 10 (* 2 5)) 5)])
      (send temperature-dc clear)
;;      (send temperature-dc draw-line start-x start-y (+ start-x 15) start-y)
      (draw-temperature-text 3 3 start-x start-y w1 w1 sw sh temperature-dc
                             temperatures))))

(define (draw-vision vision-dc)
  (let-values ([(width height) (send vision-dc get-size)])
    (let ([vision (compute-vision A env movements)]
          [sw 5.0]
          [sh 5.0]
          [w1 (/ (- width 10 (* 4 5)) 6)]
          [start-x 13]
          [start-y (/ (- height 10 (* 3 5)) 3)])
      ;;(send vision-dc clear)
      (draw-data-rectangles 1 5 start-x start-y w1 w1 sw sh vision-dc
                            (vector-take vision 5))
      (draw-data-rectangles 1 3 (+ start-x w1 sw)
                            (+ start-y w1 sh) w1 w1 sw sh vision-dc
                            (vector-take (vector-drop vision 5) 3))
      (draw-data-rectangles 1 1 (+ start-x w1 w1 sw sw)
                            (+ start-y w1 w1 sh sh) w1 w1 sw sh
                            vision-dc (vector-drop vision 8)))))

;; TODO -- the sensors are computed twice. 1) when asking the agent to act;
;; 2) when plotting sensory data
(define (draw-sensors)
  (draw-energy energy-dc)
  (draw-vision vision-dc)
  (draw-temperature temperature-dc)
  (draw-proximity proximity-dc))

(define (draw-proximity proximity-dc)
  (let-values ([(width height) (send proximity-dc get-size)])
    (let ([proximity (sense-proximity A env movements)]
          [dot-center-x (- (/ width 2) 2.5)]
          [dot-center-y (- (/ height 2) 2.5)]
          [center-x (/ width 2)]
          [center-y (/ height 2)]
          [block-length 6] ;; size of each block visualized by this sensor
          [span 6]) ;; size of the basis of each sensory triangle 
      (let ([center-x- (- center-x span)]
            [center-x+ (+ center-x span)]
            [right-x (+ center-x (* (+ 1 (& proximity 1)) block-length))]
            [left-x (- center-x (* (+ (& proximity 3) 1) block-length))]
            [center-y- (- center-y span)]
            [center-y+ (+ center-y span)]
            [up-y (- center-y (* (+ (& proximity 0) 1) block-length))]
            [down-y (+ center-y (* (+ (& proximity 2) 1) block-length))])
        (send proximity-dc clear)
        ;; (send proximity-dc set-pen "White" 0 'transparent)
        ;; (send proximity-dc set-brush "Red" 'solid)
        ;; (send proximity-dc draw-rectangle dot-center-x dot-center-y 5 5)
        (send proximity-dc set-pen "Black" 1 'solid)
        (send proximity-dc set-brush "Gray" 'solid)
        (when (< (& proximity 0) 5)
            ;; (begin 
            ;;   (send proximity-dc draw-line center-x center-y center-x up-y))
              ;;(send proximity-dc draw-text ">4" (- dot-center-x 6) 1))
              (send proximity-dc draw-polygon
                    (list
                     (make-object point% center-x center-y)
                     (make-object point% center-x- up-y)
                     (make-object point% center-x+ up-y)))
              (send proximity-dc draw-text (number->string (& proximity 0))
                    (- dot-center-x 1) 1))
        (when (< (& proximity 1) 5)
            ;; (begin 
            ;;   (send proximity-dc draw-line center-x center-y right-x center-y))
              ;;(send proximity-dc draw-text ">4" (- width 20) (- (/ height 2) 9)))
              (send proximity-dc draw-polygon
                    (list
                     (make-object point% center-x center-y)
                     (make-object point% right-x center-y-)
                     (make-object point% right-x center-y+)))
              (send proximity-dc draw-text (number->string (& proximity 1))
                    (- width 15) (- (/ height 2) 9)))
        (when (< (& proximity 2) 5)
            ;; (begin
            ;;   (send proximity-dc draw-line center-x center-y center-x down-y))
              ;;(send proximity-dc draw-text ">4" (- dot-center-x 6) (- height 17)))
              (send proximity-dc draw-polygon
                    (list
                     (make-object point% center-x center-y)
                     (make-object point% center-x- down-y)
                     (make-object point% center-x+ down-y)))
              (send proximity-dc draw-text (number->string (& proximity 2))
                    (- dot-center-x 1) (- height 17)))
        (when (< (& proximity 3) 5)
            ;; (begin 
            ;;   (send proximity-dc draw-line center-x center-y left-x center-y))
              ;;(send proximity-dc draw-text " " 0 (- (/ height 2) 9)))
          (send proximity-dc draw-polygon
                (list
                 (make-object point% center-x center-y)
                 (make-object point% left-x center-y-)
                 (make-object point% left-x center-y+)))
          (send proximity-dc draw-text (number->string (& proximity 3))
                7 (- (/ height 2) 9)))))))

;;; Saving and Loading
(define (load-environment-gui file)
  (load-and-set-environment file)
  (set! A (place-agent-randomly A env WORLD-SIZE))
  (set-global-variables)
  (send dc clear)
  (draw-grid)
  (draw-elements))

;;(make-gui)
;;(load-environment-gui "env3.txt")
;;(save-environment "env3.txt")

(define (change-size new-world-size)
  (set! WORLD-SIZE new-world-size)
  (set! env (build-environment new-world-size))
  (set! movements (make-movements new-world-size))
  (set! A (place-agent-randomly A env WORLD-SIZE))
  (set-global-variables)
  (send dc clear)
  (draw-grid)
  (draw-elements))

;;(send dc set-smoothing 'aligned)
;;(change-size 30)