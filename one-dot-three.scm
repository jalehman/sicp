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
