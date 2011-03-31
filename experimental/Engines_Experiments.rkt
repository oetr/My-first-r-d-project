(define start-timer #f)
(define stop-timer #f)
(define decrement-timer #f)
(let ((clock 0) (handler #f))
  (set! start-timer
        (lambda (ticks new-handler)
          (set! handler new-handler)
          (set! clock ticks)))

  (set! stop-timer
        (lambda ()
          (let ((time-left clock))
            (set! clock 0)
            time-left)))
  (set! decrement-timer
        (lambda ()
          (when (> clock 0)
            (begin
              (set! clock (- clock 1))
              (when (= clock 0) (handler)))))))

(define make-engine
  (let ((do-complete #f) (do-expire #f))
    (define timer-handler
      (lambda ()
        (start-timer (call/cc do-expire) timer-handler)))
    (define new-engine
      (lambda (resume)
        (lambda (ticks complete expire)
          ((call/cc
            (lambda (escape)
              (set! do-complete
                    (lambda (ticks value)
                      (escape (lambda () (complete ticks value)))))
              (set! do-expire
                    (lambda (resume)
                      (escape (lambda ()
                                (expire (new-engine resume))))))
              (resume ticks)))))))
    (lambda (proc)
      (new-engine
       (lambda (ticks)
         (start-timer ticks timer-handler)
         (let ((value (proc)))
           (let ((ticks (stop-timer)))
             (do-complete ticks value))))))))

(define-syntax timed-lambda
  (syntax-rules ()
    (( formals exp1 exp2 ...)
     (lambda formals (decrement-timer) exp1 exp2 ...))))

(define-syntax por
  (syntax-rules ()
    ((_ x ...)
     (first-true
      (list (make-engine (lambda () x)) ...)))))

(define first-true
  (lambda (engs)
    (if (null? engs)
        #f
        ((car engs) 1
         (lambda (ticks value)
           (or value (first-true (cdr engs))))
         (lambda (eng)
           (first-true
            (append (cdr engs) (list eng))))))))

(define round-robin
  (lambda (engs)
    (if (null? engs)
        '()
        ((car engs) 1
         (lambda (ticks value)
           (cons value (round-robin (cdr engs))))
         (lambda (eng)
           (round-robin
            (append (cdr engs) (list eng))))))))

(define mileage
  (lambda (thunk)
    (let loop ((eng (make-engine thunk)) (total-ticks 0))
      (eng 1
           (lambda (ticks value)
             (+ total-ticks (- 50 ticks)))
           (lambda (new-eng)
             (loop new-eng (+ total-ticks 50)))))))


(define fibonacci
  (lambda (n)
    (if (< n 2)
        n
        (+ (fibonacci (- n 1))
           (fibonacci (- n 2))))))

(round-robin
 (map (lambda (x)
        (make-engine
         (lambda ()
           (fibonacci x))))
      '(4 5 2 8 3 7 6 2)))

(mileage (lambda () (fibonacci 10)))


(define r #f)

(+ 1 (call/cc
      (lambda (k)
        (set! r k)
        (+ 2 (k 3)))))

(define list-product
  (lambda (s)
    (call/cc
     (lambda (exit)
       (let recur ((s s))
         (if (null? s) 1
             (if (= (car s) 0)
                 (exit 0)
                 (* (car s) (recur (cdr s))))))))))


(define (member x ls)
  (call/cc
   (lambda (break)
     (do ([ls ls (cdr ls)])
         ((null? ls) #f)
       (when (equal? x (car ls))
         (break ls))))))

(define-syntax test
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (if e1 (test e2 e3 ...) #f)]))

(let ([x (call/cc (lambda (k) k))])
  (x (lambda (ignore) "hi")))

(((call/cc (lambda (k) k))
  (lambda (x) x))
 "HEY!")

(define retry #f)

(define (factorial x)
  (if (= x 0)
      (call/cc (lambda (k) (set! retry k) 1))
      (* x (factorial (- x 1)))))

(define (test x)
  (cond ((< x 0) (- x))
        (else x)))

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2.0))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (double x)
  (+ x x))

(define (fact-rec n)
  (if (<= n 1)
      1
      (* n
         (fact-rec (- n 1)))))

(define (fact-iter max-count)
  (define (iter product counter)
    (if (> counter max-count)
        product
        (iter (* product counter)
              (+ counter 1))))
  (iter 1 1))


;; (let ([n 150000])
;;   (collect-garbage)
;;   (collect-garbage)
;;   (sleep 2.0)
;;   (time (fact-rec n))
;;   (collect-garbage)
;;   (collect-garbage)
;;   (sleep 2.0)
;;   (collect-garbage)
;;   (collect-garbage)
;;   (sleep 2.0)
;;   (time (fact-iter n))
;;   (collect-garbage)
;;   (collect-garbage)
;;   (void))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

