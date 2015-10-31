;; ============================================================================
;; Helpers
;; ============================================================================

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (square x)
  (* x x))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


(* (f 1) (f 2) (f 3))
(* 1 2 3)




;; ============================================================================
;; Exercise 1.29
;; ============================================================================

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))

  (define (y k)
    (f (+ a (* k h))))

  (define (coefficient k)
    (cond ((= k 0)  1.0)
          ((odd? k) 4.0)
          (else     2.0)))

  (define (si-term k)
    (* (coefficient k) (y k)))

  (* (sum si-term 0 inc n) (/ h 3.0)))

;; ============================================================================
;; Exercise 1.30
;; ============================================================================

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; ============================================================================
;; Exercise 1.31
;; ============================================================================

;; ============================================================================
;; a. Recursive Procedure

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (approx-pi n)

  (define (numerator-term x)
    (cond ((= x 0)  2.0)
          ((odd? x) (+ x 3.0))
          (else     (+ x 2.0))))

  (define (denominator-term x)
    (cond ((= x 0)   3.0)
          ((odd? x)  (+ x 2.0))
          (else      (+ x 3.0))))

  (* 4.0
     (/ (product-iter numerator-term 0 inc n)
        (product-iter denominator-term 0 inc n))))

;; ============================================================================
;; b. Iterative Procedure

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; ============================================================================
;; Exercise 1.32
;; ============================================================================

;; ============================================================================
;; a. Recursive Procedure

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

;; ============================================================================
;; a. Iterative Procedure

(define (accumulate-iter combine null-value term a next b)
  (define (iter a result)
    (if (> a b)
        null-value
        (iter (next a) (combine result (term a)))))
  (iter a null-value))

;; ============================================================================
;; Exercise 1.33
;; ============================================================================

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (accumulate combiner null-value term (next a) next b))))

;; a. (filtered-accumulate + 0 prime? square a inc b)
;; b. TODO:

;; ============================================================================
;; Exercise 1.34
;; ============================================================================

;; Some sort of error about 2 not being a function.

(define (f g)
  (g 2))

;; What is (f f) ?

;; (f f) => (f 2) => (2 2) => error?

;; (f f)                                   ; yup, call of non-procedure "2"

;; ============================================================================
;; Exercise 1.35
;; ============================================================================

;; x -> 1 + 1/x => x^2 -> x + 1, which is the definition of the golden ratio.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             2.0)

;; ============================================================================
;; Exercise 1.36
;; ============================================================================

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; takes far fewer steps with average damping than otherwise.

;; ============================================================================
;; Exercise 1.37
;; ============================================================================

;; WRONG
;; (define (cont-frac n d k)
;;   (if (= 0 k)
;;       (/ (n k) (d k))
;;       (/ (n k)
;;          (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))

  (frac 1))

;; 0.6180

;; (= k 10) for four decimal places of accuracy
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(define (cont-frac-iter n d k)

  (define (term k)
    (/ (n k) (d k)))

  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))

  (iter (- k 1) (/ (n k) (d k))))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)

;; ============================================================================
;; Exercise 1.38
;; ============================================================================

(define (euler-expansion k)

  (define (expansion i)
    (cond ((< i 0)                     0)
          ((not (= (remainder i 3) 2)) 1)
          (else
           (+ 2 (expansion (- i 3))))))

  (+ 2 (cont-frac (lambda (i) 1.0)
                  expansion
                  k)))

(euler-expansion 10)

;; n  res
;; 1  2
;; 4  4
;; 7  6
;; 10 8
;; 13 10
;; 17 12
;; else, 1
;; (remainder 5 3)

;; ============================================================================
;; Exercise 1.39
;; ============================================================================

(define (tan-cf x k)

  (define (expansion i)
    (if (= i 1)
        1
        (+ 2 (expansion (- i 1)))))

  (- (cont-frac (lambda (i) (if (= i 1) (- x) (- (* x x))))
                expansion
                k)))

;; ============================================================================
;; Exercise 1.40
;; ============================================================================

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; ============================================================================
;; Exercise 1.41
;; ============================================================================

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

;; ((double inc) 2) => 4

;; ============================================================================
;; Exercise 1.42
;; ============================================================================

(define (compose f g)
  (lambda (x) (f (g x))))

;; ((compose (lambda (x) (* x x)) inc) 6) => 49

;; ============================================================================
;; Exercise 1.43
;; ============================================================================

(define (repeated f n)

  (define (recur i)
    (if (= i 1)
        f
        (compose f (recur (- i 1)))))

  (lambda (x) ((recur n) x)))

(define (square x) (* x x))

;; ((repeated square 2) 5) => 625

;; ============================================================================
;; Exercise 1.44
;; ============================================================================

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define (n-fold-smooth f n)
  (compose (repeated f n) (smooth f)))

;; ============================================================================
;; Exercise 1.45, 1.46
;; ============================================================================

;; TODO:
