;;; Instantiate environment and agent
;; let's build an environment
(define WORLD-SIZE 10)
(define env (build-environment WORLD-SIZE))
(define movements (list->vector `(,(- WORLD-SIZE) +1 ,WORLD-SIZE -1)))
(define A (make-agent 0 0 100 100 void))
(set-agent-fn! A (random-as A actions))

(define B (make-agent 0 0 100 100 void))
(set-agent-fn! B (random-as B actions))

(set! A (place-agent-randomly A env WORLD-SIZE))

;; Create a simulator
(define STEPS 10)