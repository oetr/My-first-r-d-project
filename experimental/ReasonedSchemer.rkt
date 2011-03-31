(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define-syntax case-i
  (syntax-rules ()
    ((_ e on-zero ((â) on-one) ((a f) on-choice))
     (let ([a-i e])
       (cond
        [(not a-i) on-zero]
        [(not (and
               (pair? a-i)
               (procedure? (cdr a-i))))
         (let ([â a-i])
           on-one)]
        [else (let ([a (car a-i)]
                    [f (cdr a-i)])
                on-choice)])))))

(define map-i
  (lambda (n p a-i)
    (case-i a-i
            '()
            ((a)
             (cons (p a) '()))
            ((a f)
             (cons (p a)
                   (cond
                    ((not n) (map-i n p (f)))
                    ((> n 1) (map-i (- n 1) p (f)))
                    (else '())))))))

(define #s (lambda (s) (unit s)))