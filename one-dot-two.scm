;; "The implementation of Scheme we shall consider in Chapter 5 does not share this defect."

;; ============================================================================
;; Exercise 1.9
;; ============================================================================

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

;; =============
;; (define (+ a b)
;;   (if (= a 0) b (inc (+ (dec a) b))))
;; =============

;; (inc 1)

;; ;; 0
;; (if (= 4 0)
;;     5
;;     (inc (+ (dec 4) 5)))

;; ;; 1
;; (inc
;;  (if (= 3 0)
;;      5
;;      (+ (dec 3) 5)))

;; ;; 2
;; (inc
;;  (inc
;;   (if (= 2 0)
;;       5
;;       (+ (dec 2) 5))))

;; ;; 3
;; (inc
;;  (inc
;;   (inc
;;    (if (= 1 0)
;;        5
;;        ...))))

;; Linear recursive process: We maintain a stack of 'inc' calls until we reach 0

;; =============
;; (define (+ a b)
;;   (if (= a 0) b (+ (dec a) (inc b))))
;; =============

;; ;; 0
;; (if (= 4 0)
;;     5
;;     (+ (dec 4) (inc 5)))

;; ;; 1
;; (if (= 3 0)
;;     6
;;     (+ (dec 3) (inc 6)))

;; ;; 2
;; (if (= 2 0)
;;     7
;;     (+ (dec 2) (inc 7)))

;; Linear iterative process: We only every need to maintain the value of two state variables.

;; ============================================================================
;; Exercise 1.10
;; ============================================================================

;; =============
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10) => 1024
;; =============

(define (f n) (A 0 n)) ;; => 2n
(define (g n) (A 1 n)) ;; => 2^n
(define (h n) (A 2 n)) ;; => 2^(h(n-1))

;; (h 1) = 2^1 = 2 = 2^(h 0)
;; (h 2) = 2^2 = 4 = 2^(h 1)
;; (h 3) = 2^4 = 16 = 2^(h 2)
;; (h 4) = 2^16 = 65536 = 2^(h 16)

;; ============================================================================
;; Exercise 1.11
;; ============================================================================

;; Recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; Iterative process
(define (f n)

  (define (f-iter a b c count)
    (if (< count 3)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

  (if (< n 3)
      n
      (f-iter 2 1 0 n)))

;; ============================================================================
;; Exercise 1.12
;; ============================================================================
;; Write a procedure that computes elements of Pascal's triangle by
;; means of a recursive process.

;; NOTE: elements of the triangle are called *binomial coefficients*
;; -- nth row consists of the coefficients of the terms in the
;; expansion of (x + y)^n

(define (filter list pred)
  (cond ((= (count list) 0) list)
        (())))

(define (pascals-triangle row col)
  (cond ((= 0 col) 1)
        ((= row col) 1)
        (else (+ (pascals-triangle (- row 1) (- col 1))
                 (pascals-triangle (- row 1) col)))))

;; ============================================================================
;; Exercise 1.15
;; ============================================================================

;; TODO:

;; ============================================================================
;; Section 1.2.4 Notes
;; ============================================================================

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; (fast-expt 2 2)
;; (square (fast-expt 2 1)) ;; (/ 2 2) = 1, one is odd
;; (square (* 2 (fast-expt 2 0)))
;; (square (* 2 1)) => 4

;; (fast-expt 2 3)
;; (* 2 (fast-expt 2 2))
;; (* 2 4) => 8

;; (fast-expt 2 6)
;; (square (fast-expt 2 3))
;; (square 8) => 64


;; ============================================================================
;; Exercise 1.16
;; ============================================================================

;; Design a procedure that evolves an iterative exponentiation process
;; that uses successive squaring and uses a logarithmic number of
;; steps, as does fast-expt. (Hint: Using the observation that (b^(n/2))^2
;; = (b^2)^(n/2), keep, along with the exponent n and the base b, an
;; additional state variable a, and define the state transformation in
;; such a way that the product ab^n is unchanged from state to
;; state. At the beginning of the process a is taken to be 1, and the
;; answer is given by the value of a at the end of the process. In
;; general, the technique of defining an invariant quantity that
;; remains unchanged from state to state is a powerful way to think
;; about the design of iterative algorithms.)

(define (fast-expt-iter b n)

  (define (iter b n a)
    (cond ((= n 0)   a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else      (iter b (- n 1) (* b a)))))

  (iter b n 1))

;; (iter 2 1 1) => (iter 2 0 (* 2 1)) => (iter 2 0 2) => 2
;; (iter 2 2 1) => (iter (square 2) (/ 2 2) a) => (iter 4 1 1) => (iter 4 0 (* 1 4)) => 4
;; (iter 2 3 1) => (iter 2 2 2) => (iter 4 1 2) => 8

;; ============================================================================
;; Exercise 1.17
;; ============================================================================

(define (slow-mult a b)
  (if (= b 0)
      0
      (+ a (slow-mult a (- b 1)))))

;; (slow-mult 0 5)

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (fast-mult a b)

  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else      (+ a (fast-mult a (- b 1))))))

;; (fast-mult 2 4) => (double (fast-mult 2 2)) => (double (double (fast-mult 2 1))) =>
;; (double (double (+ 2 (fast-mult 2 0)))) => (double (double 2)) => 8

;; (fast-mult 8 8)

;; ============================================================================
;; Exercise 1.18
;; ============================================================================

(define (fast-mult-iter a b)

  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else      (iter a (- b 1) (+ acc a)))))

  (iter a b 0))

;; (fast-mult-iter 3 4)

;; ============================================================================
;; Exercise 1.19
;; ============================================================================

;; TODO:

;; ============================================================================
;; Exercise 1.20
;; ============================================================================

;; ============================================================================
;; Section 1.2.6 Notes
;; ============================================================================

(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; (divides? 2 5)
;; (find-divisor 27 4)

(define (prime? n)
  (= n (smallest-divisor n)))

;; ============================================================================
;; Exercise 1.21
;; ============================================================================

;; (smallest-divisor 199) => 199

;; (find-divisor 199 2) =>
;; (find-divisor 199 3) => ...19

;; (smallest-divisor 1999) => 1999
;; (smallest-divisor 19999) => 7?

;; ============================================================================
;; Exercise 1.22
;; ============================================================================

(define (not= a b)
  (not (= a b)))

(define runtime current-milliseconds)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

;; (timed-prime-test 1999)

(define (search-for-primes start num)

  (define (iter n counter)
    (if (not= counter num)
        (if (timed-prime-test n)
            (iter (+ n 2) (+ counter 1))
            (iter (+ n 2) counter))))

  (if (even? start)
      (iter (+ 1 start) 0)
      (iter start 0)))

;; And yeah, the timing checks out.

;; (search-for-primes 100000000 3) => ~4ms
;; (search-for-primes 1000000000 3) => ~13ms
;; (search-for-primes 10000000000 3) => ~43ms
;; (search-for-primes 100000000000 3) => ~142ms
;; (search-for-primes 1000000000000 3) => ~445ms

;; ============================================================================
;; Exercise 1.23
;; ============================================================================

;; NOTE: don't define a procedure inside of a procedure that will be
;; called recursively! That will require the procedure to be redefined
;; numerous times, resulting in a big slowdown!

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; (search-for-primes 100000000 3) => ~5ms => 0.8x
;; (search-for-primes 1000000000 3) => ~9ms => 1.44x
;; (search-for-primes 10000000000 3) => ~28ms => 1.5x
;; (search-for-primes 100000000000 3) => ~86ms => 1.65x
;; (search-for-primes 1000000000000 3) => ~280ms => 1.59x

;; Expectation mostly confirmed. Expecting doubled performance, got
;; about 1.5x better performance. Guessing this has to do with the
;; extra calls introduced by the 'next' function -- it's always two
;; calls more than just a simple addition.

;; ============================================================================
;; Exercise 1.24
;; ============================================================================

(define true #t)
(define false #f)

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
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))
      #f))

;; (search-for-primes 100000000 3) => ~0ms
;; (search-for-primes 1000000000 3) => ~0ms
;; (search-for-primes 10000000000 3) => ~28ms => 1.5x ;; keeps hanging on me when I attempt to execute this
;; (search-for-primes 100000000000 3) => ~86ms => 1.65x
;; (search-for-primes 1000000000000 3) => ~280ms => 1.59x

;; ============================================================================
;; Exercise 1.25
;; ============================================================================

;; The existing implementation makes use of the following property
;; from footnote 46: "...for any integers x, y, and m, we can find the
;; remainder of x times y modulo m by computing separately the
;; remainders of x modulo m and y modulo m, multiplying these, and
;; then taking the remainder of the result modulo m."

;; This keeps the number that we're squaring/multiplying relatively
;; small. As the `fermat-test' alogrithm is primarily usfeful for
;; testing really, really, really large numbers, we don't want to be
;; doing things like *squaring* them. Instead, we work with their
;; remainders.

;; ============================================================================
;; Exercise 1.26
;; ============================================================================

;; By replacing (square (expmod base (/ exp 2) m)) with
;; (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) Louis has introduced
;; exponential growth. While n is shrinking logarithmically, every n/2 steps
;; the number of calls to expmod grows by 2^n. The two processes can be considered
;; to cancel each other out, leaving us with a O(n) process
;; (can't currently figure out how to get a big theta...pretend that O is one).

;; ============================================================================
;; Exercise 1.27
;; ============================================================================

(define (test-carmichael n)
  (define (iter a n result)
    (cond ((= a n) result)
          ((= (expmod a n n) a) (iter (+ a 1) n true))
          (else false)))

  (iter 1 n true))

(test-carmichael 561) ;; true
(test-carmichael 1105) ;; true
(test-carmichael 1729) ;; true

;; ============================================================================
;; Exercise 1.28
;; ============================================================================

;; "nontrivial square root of 1 modulo n"
(define (nontrivial-one-root n)
  (and (not= n 1)
       (not= n (- n 1))
       (= (square n) (remainder 1 n))))

(define (expmod-signal base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-signal a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))

// TODO:
