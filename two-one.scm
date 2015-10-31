;; ============================================================================
;; Helpers

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))

(define (average a b)
  (/ (+ a b) 2))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; ============================================================================
;; Exercise 2.1
;; ============================================================================

;; Define a better version of make-rat that handles both positive and
;; negative arguments. Make-rat should normalize the sign so that if
;; the rational number is positive, both the numerator and denominator
;; are positive, and if the rational number is negative, only the
;; numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (cond ((and (negative? n) (negative? d)) (cons (abs n) (abs d)))
            ((negative? n) (cons n d))
            ((negative? d) (cons (- n) (abs d)))
            (else (cons n d))))))

(define one-half (make-rat -1 2))
(define one-third (make-rat 1 3))

;; ============================================================================
;; Exercise 2.2
;; ============================================================================

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((mid-x (average (x-point (start-segment segment))
                        (x-point (end-segment segment))))
        (mid-y (average (y-point (start-segment segment))
                        (y-point (end-segment segment)))))
    (make-point mid-x mid-y)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(let ((start (make-point 2 2))
      (end   (make-point 3 7)))
  (print-point (midpoint-segment (make-segment start end))))

;; ============================================================================
;; Exercise 2.3
;; ============================================================================

;; make a rectangle in terms of a Lower Left Corner and an Upper Right
;; Corner (points)
(define (make-rect llc urc)
  (let ((height (abs (- (y-point urc) (y-point llc))))
        (width  (abs (- (x-point llc) (x-point urc)))))
    (cons height width)))

(define (make-rect height width)
  (cons height width))

(define (width rect) (cdr rect))

(define (height rect) (car rect))

(define (perimeter rect)
  (+ (* 2 (width rect)) (* 2 (height rect))))

(define (area rect)
  (* (height rect) (width rect)))

;; (let ((llc (make-point 8 20))
;;       (urc (make-point 1 1)))
;;   (area (make-rect 4 25)))

;; ============================================================================
;; Exercise 2.4
;; ============================================================================

;; (define (cons x y)
;;   (define (dispatch m)
;;     (cond ((= m 0) x)
;;           ((= m 1) y)
;;           (else (error "Argument not 0 or 1 -- CONS" m))))
;;   dispatch)

;; (define (car z) (z 0))

;; (define (cdr z) (z 1))

;; ;; whoa
;; ;; (cdr (cons 5 6))

;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car z)
;;   (z (lambda (p q) p)))

;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; (cdr (cons 1 2)) => 2
;; (cdr (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) q))
;; ((lambda (p q) q) 1 2)
;; 2

;; ============================================================================
;; Exercise 2.5
;; ============================================================================

;; (1 2) => ((* 2^0 3^0) ((* 2^1 3^0)))
;; (3 4) => ((* 2^0 3^1) ((* 2^2 3^0)))
;; (5 6) => ((* ...))

;; (1 2) => (* 2^1 3^2) => (* 2 9) => 18
;; => can divide 18 by two once until non-zero remainder
;; => can divide 18 by three twice until non-zero remainder

;; (3 4) => (* 2^3 3^4) => (* 8 81) => 648
;; => can divide 648 by two three times until non-zero remainder
;; => can divide 648 by three four times until non-zero remainder

;; (define (cons a b)
;;   (* (expt 2 a) (expt 3 b)))

;; (define (car z)
;;   (if (> (remainder z 2) 0)
;;       0
;;       (+ 1 (car (/ z 2)))))

;; (define (cdr z)
;;   (if (> (remainder z 3) 0)
;;       0
;;       (+ 1 (cdr (/ z 3)))))

;; ============================================================================
;; Exercise 2.6
;; ============================================================================

(define zero (lambda (f) (lambda (x) x)))
(define one  (lambda (f) (lambda (x) (f x))))
(define two  (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero)
;; (add-one (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f)
;;   (lambda (x)
;;     (f ((lambda (x) x) x))))
;; => (lambda (f) (lambda (x) (f x)))

;; (add-1 one)
;; (add-1 (lambda (f) (lambda (x) (f x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; => (lambda (f) (lambda (x) (f (f x))))

;; TODO:
;; (define (plus a b)
;;   )

;; ============================================================================
;; Exercise 2.7
;; ============================================================================

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; ============================================================================
;; Exercise 2.8
;; ============================================================================

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;; ============================================================================
;; Exercise 2.9
;; ============================================================================

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define interval-a (make-interval 1 2))
(define interval-b (make-interval 0.75 1.5))

(add-interval interval-a interval-b)
(sub-interval interval-a interval-b)

;; true
(= (+ (width interval-a)
      (width interval-b))

   (width (add-interval interval-a interval-b)))

;; true
(= (- (width interval-a)
      (width interval-b))

   (width (sub-interval interval-a interval-b)))

;; false
(= (* (width interval-a)
      (width interval-b))

   (width (mul-interval interval-a interval-b)))

;; false
(= (/ (width interval-a)
      (width interval-b))

   (width (div-interval interval-a interval-b)))

;; ============================================================================
;; Exercise 2.10
;; ============================================================================

(define (spans-zero? interval)
  (and (<= (lower-bound interval) 0)
       (>= (upper-bound interval) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Attempt to divide by interval that spans zero -- DIV-INTERVAL" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; (div-interval interval-a (make-interval -2 2))

;; ============================================================================
;; Exercise 2.11
;; ============================================================================

;; TODO:

;; ============================================================================
;; Exercise 2.12
;; ============================================================================

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(percent (make-interval 3.35 3.65))

(define (make-center-percent c pt)
  (make-center-width c (* (/ pt 100.0) c)))

;; ============================================================================
;; Exercise 2.13-2.16
;; ============================================================================

;; TODO:
