(define (memq item xs)
  (cond ((null? xs) #f)
        ((eq? item (car xs)) xs)
        (else (memq item (cdr xs)))))

(memq 'a '(a b c))

;; ===============================================================
;; Exercise 2.53
;; ===============================================================

(list 'a 'b 'c) ; => (a b c)
(list (list 'george)) ; => ((george))
(cdr '((x1 x2) (y1 y2))) ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; => (y1 y2)
(pair? (car '(a short list))) ; => false
(memq 'red '((red shoes) (blue sockes))) ; => false
(memq 'red '(red shoes blue socks)) ; => (red shoes blue socks)

;; ===============================================================
;; Exercise 2.54
;; ===============================================================

(define (equal? xs ys)
  (cond ((and (null? xs) (null? ys)) #t)
        ((not (= (length xs) (length ys))) #f)
        ((eq? (car xs) (car ys)) (equal? (cdr xs) (cdr ys)))
        (else #f)))

(equal? '(a c) '(a b c))

;; ===============================================================
;; Exercise 2.55
;; ===============================================================

(car ''abracadabra)

;; this expands to:
(car (quote (quote abracadabra)))
;; which means that we're quoting the list (quote abracadabra), the first
;; element of which is "quote"

;; ===============================================================

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (comp f g)
  (lambda (x)
    (f (g x))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m2 0) (=number? m1 0)) 0)
        ((and (number? m1) (number? m2) (* m1 m2)))
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 0) 0)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

;; 2.58a
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))
;; 2.57
;; (define (augend s)
;;   (if (null? (cdddr s))
;;       (cadr s)
;;       (cons '+ (cddr s))))

;; 2.58a
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier x) (car x))
(define (multiplicand x) (caddr x))
;; 2.57
;; (define (multiplicand x)
;;   (if (null? (cdddr x))
;;       (cadr x)
;;       (cons '* (cddr x))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))

        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))

        ((exponentiation? exp)
         (make-product
          (deriv (base exp) var)
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (make-sum
                                            (exponent exp)
                                            (- 1))))))

        (else
         (error "unknown expression type: DERIV" exp))))

;; TODO: 2.58b
;; This is really hard.

;; ===============================================================
;; Exercise 2.59
;; ===============================================================

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(union-set '(1 2 6) '(4 1 6))

;; ===============================================================
;; Exercise 2.60
;; ===============================================================

;; element-of-set? is the same

(define (adjoin-set x set)
  (cons x set))

;; intersection-set stays the same

(define (union-set set1 set2)
  (append set1 set2))


;; Applications where this implementation would be best are those that are heavy
;; in adjoins and unions -- they're O(1).

;; ===============================================================
;; Exercise 2.61
;; ===============================================================

(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set))
         (cons (car set) (adjoin-set-ordered x (cdr set))))
        (else (cons x set))))

(adjoin-set-ordered 4 '(2 3 5 7))

;; ===============================================================
;; Exercise 2.62
;; ===============================================================

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              (else
               (intersection-set set1 (cdr set2)))))))

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set-ordered (cdr set1) set2)))
                      (else
                       (cons x2 (union-set-ordered set1 (cdr set2)))))))))

(union-set-ordered '(1 2 3) '(4 5 6))

(union-set-ordered '(4 7 9) '(4 5 6))

;; ===============================================================
;; Tree representation of sets
;; ===============================================================

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (else ;; greater than
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; ===============================================================
;; Exercise 2.63
;; ===============================================================

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; (copy-to-list (make-tree 3 ...) (cons 7 (copy-to-list (make-tree 9 ...) '())))
;; (copy-to-list (make-tree 3 ...) (cons 7 (cons 9 (copy-to-list (make-tree 11 '() '())))))
;; (copy-to-list (make-tree 3 ...) '(7 9 11))
;; (copy-to-list (make-tree 1 '() '())
;;               (cons 3 (copy-to-list (make-tree 5 '() '()))) '(7 9 11))

(define tree-1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define tree-2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))

(define tree-3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

;; a) The two procedures produce the same lists for all trees -- they produce
;; '(1 3 5 7 9 11).

;; b) Each procedure visits each node exactly once. The second procedure calls
;; cons on each element, so it runs in O(n) steps. The first procedure will call
;; `append' for each recursive call, which grows proportionally to the length of
;; the first list. Because the first list is always the left half of the tree,
;; it should be called on roughly half of the elements in the set. The first
;; procedure grows at O(nlog n)

;; ===============================================================
;; Exercise 2.64
;; ===============================================================

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; a) We start by figuring out how many elements should be in the left half and
;; right half of the tree -- the number of elements minus one divided by two is
;; the number of elements that should be present in the left half of the tree.
;; The number of elements that should be present in the right half of the tree
;; is then calculated by subtracting the `left-size' plus one from the total
;; number of elements. This ensures that we leave one element in the center of
;; the set for the entry. The left tree is computed by calling `partial-tree`
;; recursively with all of the elements and the `left-size', and the right tree
;; by calling `partial-tree' recursively with the remaining elements (returned
;; from the recursive call in a pair) and the `right-size'. Finally, we create a
;; new tree with the middle element and the left and right trees computed with
;; recursive calls to `partial-tree'.

;; b) We end up with a single `cons' for each element in the list, so growth is
;; O(n).

;; ===============================================================
;; Exercise 2.65
;; ===============================================================

(define (union-set tree1 tree2)
  (list->tree (union-set-ordered (tree->list-2 tree1)
                                 (tree->list-2 tree2))))

(define (intersection-set tree1 tree2)
  (list->tree (intersection-set-ordered (tree->list-2 tree1)
                                        (tree->list-2 tree2))))

;; TODO: This seems too easy. Would a better challenge be to have the sets given
;; as lists, and then internally converted to trees and processed as such? Here
;; we're just using the tree ops to convert potentially unordered lists into
;; ordered ones efficiently, and using our ordered set algorithms for that.
;; Seems sensible.

;; ===============================================================
;; Exercise 2.66
;; ===============================================================

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= (key (entry set-of-records)) given-key)
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))

