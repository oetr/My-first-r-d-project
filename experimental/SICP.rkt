;;; Chapter 1
(require (planet williams/science/random-distributions/flat))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1  (random (- n 1)))))

(define (fast-prime? n)
  (define (fast-prime-aux times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime-aux (- times 1)))
          (else false)))
  (fast-prime-aux 100))

;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes start)
  (define (consecutive-primes counter n 3-primes)
    (cond ((= counter 3) (void))
          ((timed-prime-test n) (consecutive-primes (+ counter 1) (+ n 2) (cons n 3-primes)))
          (else (consecutive-primes counter (+ n 2) 3-primes))))
  (if (odd? start)
      (consecutive-primes 0 start empty)
      (consecutive-primes 0 (+ start 1) empty)))

(define (sum term a next b)
  (define (sum-aux result a)
    (if (> a b)
        result
        (sum-aux (+ result (term a)) (next a))))
  (sum-aux 0 a))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube n) (* n n n))

(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-integral f a b n)
  (let ([h (/ (- b a) n)])
    (define (term k)
      (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
            ((even? k) (* 4.0 (f (+ a (* k h)))))
            (else (* 2.0 (f (+ a (* k h)))))))
    (* (/ h 3.0) (sum term 0.0 inc n))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0(* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (clean)
  (for ((i (in-range 5)))
       (collect-garbage))
  (sleep 2))

(define (product f a next b)
  (define (product-acc result a)
    (if (> a b)
        result
        (product-acc (* result (f a)) (next a))))
  (product-acc 1 a))

(define (product-pi end)
  (* 2 (+ end 2) (product (lambda (a) (/ (square a) (square (+ a 1))))
                          2.0
                          (lambda (a) (+ a 2))
                          end)))

(define (accumulate combiner null-value f a next b)
  (define (accumulate-aux result a)
    (if (> a b)
        result
        (accumulate-aux (combiner result (f a)) (next a))))
  (accumulate-aux null-value a))

(define (filtered-accumulate test-passed? combiner null-value f a next b)
  (define (filtered-accumulate-aux result a)
    (if (> a b)
        result
        (if (test-passed? a)
            (filtered-accumulate-aux (combiner result (f a)) (next a))
            (filtered-accumulate-aux result (next a)))))
  (filtered-accumulate-aux null-value a))

(define (cont-frac combiner n d k)
  (define (cont-frac-aux result i)
    (if (= i 0)
        result
        (begin
;;          (printf "i=~a, d=~a, n=~a, resu=~a~n" i (d i) (n i) result)
          (cont-frac-aux (/ (n i) (combiner (d i) result)) (- i 1)))))
  (cont-frac-aux 0 k))

(define (cont-frac-rec n d k)
  (define (cont-frac-rec-aux i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-rec-aux (+ i 1))))))
  (cont-frac-rec-aux 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (square x)))
  (cont-frac - n (lambda (i) (+ (* 2 (- k i)) 1)) k))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next )
          next
          (try next))))
  (try first-guess))

(define (my-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newtons-sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (compose g f)
  (lambda (x) (g (f x))))

(define (double f)
  (lambda (x) (f (f x))))


(let ([line-number 0])
  (define (my-print x)
    (display line-number)
    (display x)
    (set! line-number (+ line-number 1)))
  (void))

(define my-print
  (let ([line-number 0])
    (lambda (x)
      (printf "~a\n~a\n" line-number x)
      (set! line-number (+ line-number 1))
      '())))



;;; chapter 2
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      empty
      (cons low (enumerate-interval (+ low 1) high))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))empty sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (my-foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (my-rev1 sequence)
  (foldl (lambda (x y) (cons x y)) empty sequence))

(define (my-rev2 sequence)
  (accumulate (lambda (x y) (append y  (list x))) empty sequence))

(define (my-rev3 sequence)
  (fold-left (lambda (x y) (cons y x)) empty sequence))

;; Nested mappings
(accumulate append
            empty
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 4)))

(define (flatmap proc seq)
  (accumulate append empty (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (first pair) (second pair))))

(define (make-pair-sum pair)
  (list (first pair)
        (second pair)
        (+ (first pair) (second pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (my-remove item sequence)
  (filter (lambda (i) (not (= i item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list empty)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (my-remove x s))))
               s)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-equal-to n)
  (filter (lambda (seq) (odistinct?
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval 1 n)))
                              (enumerate-interval 1 n)))
                   (enumerate-interval 1 n))))

(define (distinct? a-list)
  (define (distinct?-acc item a-list)
    (if (empty? a-list)
        #t
        (and (not (equal? item (first a-list)))
             (distinct?-acc item (rest a-list)))))
  (if (empty? a-list)
      #t
      (distinct?-acc (first a-list) (rest a-list))))

