;;; Libraries
(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/flat))
;;(require sgl sgl/gl-vectors)
(require sgl/gl)
(require racket/date)

;;; Testing OpenGL

(define gng-dc #f)
;; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define yellow-pen (make-object pen% "yellow" 2 'solid))
(define blue-pen (make-object pen% "RED" 2 'solid))
(define black-pen (make-object pen% "BLACK" 1 'solid))

(define (f)
  ;; Make a 300 x 300 frame
  (define frame (new frame% [label "Plotting GNG"]
                     [width 700]
                     [height 700]))
  ;; Make the drawing area
  (define canvas (new canvas% [parent frame]))
  ;; Get the canvas's drawing context
  (set! gng-dc (send (send canvas get-dc) get-gl-context))
  ;; Show the frame
  (send frame show #t)
  ;; Wait a second to let the window get ready
  (sleep/yield 1))

(send gng-dc call-as-current
      (lambda ()
        (glClearColor 0 0 1 1)
        (glClear GL_COLOR_BUFFER_BIT)))

(send gng-dc swap-buffers)


(glClearColor 1 0 0 1)
(glClear GL_COLOR_BUFFER_BIT)




;;; Example taken from Noel Welsh
(require sgl/gl sgl/gl-vectors)

(define (resize w h)
  (glViewport 0 0 w h)
  #t)

(define (draw-opengl)
  (glClearColor 1. 1. 1. 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3d 1.0 1.0 1.0)
  (printf "drawing~n")
  (glBegin GL_TRIANGLES)
  (glVertex3i 1 2 3)
  (glVertex4fv (gl-float-vector 1 2 3 4))
  (glEnd))

;; (glClearColor 0.0 0.0 0.0 0.0)
  ;; (glClear GL_COLOR_BUFFER_BIT)
  ;; (glColor3d 1.0 1.0 1.0)

  ;; (glMatrixMode GL_PROJECTION)
  ;; (glLoadIdentity)
  ;; (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  ;; (glMatrixMode GL_MODELVIEW)
  ;; (glLoadIdentity)
  
  ;; (glBegin GL_QUADS)
  ;; (glVertex3d 0.25 0.25 0.0)
  ;; (glVertex3d 0.75 0.25 0.0)
  ;; (glVertex3d 0.75 0.75 0.0)
  ;; (glVertex3d 0.25 0.75 0.0)
  ;; (glEnd))


(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (draw-opengl)
         (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height))))
    (super-instantiate () (style '(gl)))))

(define win (new frame%
                 [label "OpenGl Test"]
                 [min-width 200]
                 [min-height 200]))

(define gl (new my-canvas% [parent win]))

(send win show #t)
