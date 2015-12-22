;; Helpers

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (= a b)
        result
        (iter (next a) (+ result (term result)))))
  (iter a 0))

(define (inc x) (+ x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1.29

(define (simpson-integral f a b n)

  (define h (/ (- b a) n))

  (define (y k)
    (f (+ a (* k h))))

  (define (term k)
    (* (y k)
       (cond ((= k 0)   1.0)
             ((= k n)   1.0)
             ((even? k) 2.0)
             (else      4.0))))

  (if (even? n)
      (* (sum term a inc n) (/ h 3.0))
      (simpson-integral f a b (- n 1.0))))

(define (cube x)
  (* x x x))

(simpson-integral cube 0 1.0 6.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(= (product identity 1 inc 4)
   (product-iter identity 1 inc 4))

(define (factorial n)
  (product identity 1 inc n))

(define (approx-pi n)
  (define (term a)
    (/ (if (even? a) (+ a 2) (+ a 3))
       (if (even? a) (+ a 3) (+ a 2))))

  (* 4 (product-iter term 0 inc n)))

;; (approx-pi 1000000) ;; awww, yeah

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product-acc-iter term a next b)
  (accumulate-iter * 1 term a next b))

(= (product-acc identity 1 inc 5)
   (product identity 1 inc 5)
   (product-acc-iter identity 1 inc 5))          ; sweet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.33

(define (filtered-accumulate combiner null-value filter term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner
                     (term a)
                     (filtered-accumulate combiner null-value filter term (next a) next b)))
        (else (filtered-accumulate combiner null-value filter term (next a) next b))))

;; a. (filtered-accumulate + 0 prime? square a inc b)
;; b. TODO:
