;;; Some Speed Tests for Different Versions of the Same Function
;; original version

(define (substitute1 new old l)
  (cond [(null? l) '()]
        [(eq? (car l) old) (cons new (substitute1 new old (cdr l)))]
        [else (cons (car l) (substitute1 new old (cdr l)))]))

(define (substitute11 new old l)
  (if (null? l) '()
      (let ([fst (car l)])
        (cons (if (eq? fst old)
                  new
                  fst)
              (substitute11 new old (cdr l))))))

;; letrec version (implicitly through a named-let)
(define (substitute2 new old l)
  (let loop ([l l])
    (cond [(null? l) '()]
          [(eq? (car l) old) (cons new (loop (cdr l)))]
          [else (cons (car l) (loop (cdr l)))])))

;; making the code a little more compact
(define (substitute3 new old l)
  (let loop ([l l])
    (if (null? l)
        '()
        (cons (let ([fst (car l)]) (if (eq? fst old) new fst))
              (loop (cdr l))))))

;; a tail recursive version
(define (substitute4 new old l)
  (let loop ([l l] [r '()])
    (if (null? l)
        (reverse r)
        (loop (cdr l)
              (cons (let ([fst (car l)]) (if (eq? fst old) new fst)) r)))))

(define (substitute5 new old l)
  (define (acc l r)
    (if (null? l)
        (reverse r)
        (acc (cdr l)
             (cons (let ([fst (car l)])
                     (if (eq? fst old) new fst))
                   r))))
  (acc l empty))

(define substitute6
  (lambda (new old l)
    (letrec ((acc
              (lambda (l result)
                (if (null? l)
                    (reverse result)
                    (acc (cdr l)
                         (cons (let ([first (car l)])
                                 (if (eq? first old) new first))
                               result))))))
      (acc l empty))))

(define substitute7
  (lambda (new old a-list)
    (define (substitute7-aux list)
      (cond [(null? list) empty]
            [else
             (cons
              (let ([first-element (car list)])
                (if (eq? (car list) old)
                    new
                    first-element))
              (substitute7-aux (cdr list)))]))
    (substitute7-aux a-list)))

(substitute7 1 10 '(2 10 3))

;; tests and timings

(define (rand-list n)
  (if (zero? n) '() (cons (random 10) (rand-list (sub1 n)))))


(define (my-test tests functions . args)
  (for ([i functions])
       (time* tests (apply i args))))

(my-test 100 (list 
              substitute5
              substitute11
              substitute1
              substitute2
              substitute3
              substitute4
              substitute6
              substitute7)
         10 1 (make-list 100000 1))


(define fn-list
  (list
   substitute1
   substitute11
   substitute2
   substitute3
   substitute4
   substitute5
   substitute6
   substitute7))

(define (test-my-functions n list-size fn-list)
  (define l  (rand-list list-size))
  (define new (random 10))
  (define old (random 10))
  (define-syntax-rule (run fun i)
    (begin (printf "~a: ~a ~n" i (object-name fun))
           (time* n (fun new old l))))
  ;; don't time the first one, since it allocates a new list to use later  
  (define new-list (substitute1 new old l))
  (newline)
  (for ([fn fn-list] [i (in-range (length fn-list))])
       (run fn i))
  (newline))

(test-my-functions 100 100000 fn-list)
