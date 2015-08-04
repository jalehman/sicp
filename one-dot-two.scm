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

(inc 1)

;; 0
(if (= 4 0)
    5
    (inc (+ (dec 4) 5)))

;; 1
(inc
 (if (= 3 0)
     5
     (+ (dec 3) 5)))

;; 2
(inc
 (inc
  (if (= 2 0)
      5
      (+ (dec 2) 5))))

;; 3
(inc
 (inc
  (inc
   (if (= 1 0)
       5
       ...))))

;; Linear recursive process: We maintain a stack of 'inc' calls until we reach 0

;; =============
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))
;; =============

;; 0
(if (= 4 0)
    5
    (+ (dec 4) (inc 5)))

;; 1
(if (= 3 0)
    6
    (+ (dec 3) (inc 6)))

;; 2
(if (= 2 0)
    7
    (+ (dec 2) (inc 7)))

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

(define (f n) (A 0 n)) => 2n
(define (g n) (A 1 n)) => 2^n
(define (h n) (A 2 n)) => 2^(h(n-1))

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
