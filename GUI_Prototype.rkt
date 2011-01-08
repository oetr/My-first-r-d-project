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

;; A frame
(define frame (new frame% [label ""]))
(define black-pen #f)
(define white-pen #f)
(define white-brush #f)
(define black-brush #f)
(define red-brush #f)
(define dc #f)
(define slider (new slider% [label ""] [min-value 0] [max-value 10] [parent frame]))
(define step-button #f)

(define (init-frame w h)
  ;; Make a w x h frame
  (set! frame (new frame% [label "ValueSim"]
                     [width w]
                     [height (+ h 22)]))
  ;; Make the drawing area
  (define canvas (new canvas% [parent frame]))
  ;; Get the canvas's drawing context
  (set! dc (send canvas get-dc))
  ;; Make some pens and brushes
  (set! white-pen (make-object pen% "WHITE" 1 'solid))
  (set! black-pen (make-object pen% "BLACK" 0.75 'solid))
  (set! white-brush (make-object brush% "WHITE" 'solid))
  (set! black-brush (make-object brush% "BLACK" 'solid))
  (set! red-brush (make-object brush% "RED" 'solid))
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
                                     (erase frame)
                                     (draw-world)))))
  (set! slider (new slider% [parent panel]
                    [label "Delay (ms)"]
                    [min-value 30]
                    [max-value 500]))
  ;; draw frame
  (send frame show #t)
  (send dc set-smoothing 'unsmoothed)
  (sleep/yield 1))

(send dc set-smoothing 'smoothed)
(set! black-pen (make-object pen% "BLACK" 4 'solid))

(define (toggle-button)
  (define (sim)
    (step)
    (erase frame)
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
            (thread-suspend simulation))
          (begin
            (send button set-label "Stop")
            (thread-resume simulation)))
      (set! switch (not switch)))))

;; the size of the frame depends on the size of the world?
;; or make the size of the frame constant and unchangable? -- yes
(define (create-view)
  (init-frame 500 500))

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


(define (draw-wall x y width height)
  (let ([brush (send dc get-brush)]
        [pen (send dc get-pen)])
    (send dc set-brush black-brush)
    (send dc set-pen black-pen)
    (send dc draw-rectangle x y width height)
    (send dc set-brush brush)
    (send dc set-pen pen)))

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
(position->coordinates WORLD-SIZE 900)
(sqr WORLD-SIZE)

(define (draw-elements)
  (let-values ([(width height) (send dc get-size)])
    (let ([w (/ width WORLD-SIZE)]
          [h (/ height WORLD-SIZE)])
      (for ([i (in-range 0 (sqr WORLD-SIZE))])
           (let ([posn (position->coordinates WORLD-SIZE i)])
             (draw-object (& env i) posn w h)))
      (draw-agent A w h))))

(define (draw-object object posn w h)
  (cond [(and (hash? (tile-object-on-top object))
              (symbol=? 'rock (hash-ref (tile-object-on-top object) 'name)))
         (draw-wall (* w (posn-x posn)) (* h (posn-y posn)) w h)]
        [else (void)]))

(define (draw-agent agent width height)
  (let ([brush (send dc get-brush)]
        [pen (send dc get-pen)]
        [posn (position->coordinates WORLD-SIZE (agent-position A))])
    (send dc set-brush red-brush)
    (send dc set-pen black-pen)
    (send dc draw-ellipse (* width (posn-x posn)) (* height (posn-y posn)) width height)
    (send dc set-brush brush)
    (send dc set-pen pen)))

(define (make-gui)
  (create-view)
  (draw-grid)
  (draw-elements))

(define (draw-world)
  (draw-grid)
  (draw-elements))

(define (erase frame)
  (let ([brush (send dc get-brush)]
        [pen (send dc get-pen)])
    (send dc set-brush white-brush)
    (send dc set-pen white-pen)
    (send dc draw-rectangle
          0
          0
          (send frame get-width)
          (send frame get-height))
    (send dc set-brush brush)
    (send dc set-pen pen)))
;;; Add buttons
;; - start
;; - stop
;; - step
;; - slower -- faster

;; connect the buttons to their right functions

(define-syntax my-let 
  (syntax-rules ()
    [(let ([var rhs] ... ) body)
     ((lambda (var ...) body) rhs ...)]))

(define-syntax (my-let-1 stx)
  (syntax-case stx ()
    [(my-let-1 ([var rhs] ...) body)
     ;; Guard expression
     (and (andmap identifier? (syntax->list #'(var ...)))
          (not (check-duplicate #'(var ...))))
     ;; Transformation expression
     #'((lambda (var ...) body) rhs ...)]))

(define-syntax (my-let-2 stx)
  (syntax-parse stx
                [(my-let-2 ([var:identifier rhs:expr] ...) body:expr)
                 #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                 "DUPLICATE NAME"
                 #'((lambda (var ...) body) rhs ...)]))

(define-syntax (macro stx)
  (syntax-parse stx
                [(_ f:foo) #'(+ f.a f.b f.c)]))

                 