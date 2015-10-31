;; ============================================================================
;; Helpers

;; ============================================================================
;; Exercise 2.17
;; ============================================================================

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (if (= (length items) 1)
      (car items)
      (last-pair (cdr items))))

;; (last-pair (list 1))

;; ============================================================================
;; Exercise 2.18
;; ============================================================================

(define (reverse xs)
  (if (= (length xs) 1)
      xs
      (append (reverse (cdr xs)) (list (car xs)))))

;;(reverse (list 1 2 3 4))
;;(reverse (list 1 2))

;; ============================================================================
;; Exercise 2.19
;; ============================================================================

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coins)
  (null? coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)))))

;;(define us-coins (list 1 50 5 10 25))

;; (cc 100 us-coins)

;; The order does not matter -- the entire list of coins will always
;; be processed.

;; ============================================================================
;; Exercise 2.20
;; ============================================================================

;; (define (same-parity . xs)

;;   (define (r f xs)
;;     (cond ((null? xs) xs)
;;           ((f (car xs)) (cons (car xs) (r f (cdr xs))))
;;           (else (r f (cdr xs)))))

;;   (if (even? (car xs))
;;       (r even? xs)
;;       (r odd?  xs)))

;; (same-parity 1 2 3 4 5 6 7)

;; ============================================================================
;; Exercise 2.21
;; ============================================================================

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; (square-list (list 1 2 3 4))

;; ============================================================================
;; Exercise 2.22
;; ============================================================================

;; Louis' first approach involves repeatedly `cons'ing the square of
;; the car of the cdr of elements to the previous result:
;; 1. (cons (square 1) nil)
;; 2. (cons (square 2) (list 1))
;; 3. (cons (square 3) (list 4 1))
;; The new element is always placed at the *beginning* of the answer list.

;; Louis' second approach interchanges the arguments, but this means
;; that now the first argument to `cons' is a list and the second
;; argument is the value -- this will result in a list of pairs. A
;; solution would be to use the `append' function in place of `cons'
;; in conjunction with placing the next value (the squared term)
;; inside a list: `(append answer (list (square (car things))))'

;; ============================================================================
;; Exercise 2.23
;; ============================================================================

(define (for-each f items)
  (if (null? items)
      #t
      ((lambda (x)
         (f x)
         (for-each f (cdr items)))
       (car items))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; ============================================================================
;; Exercise 2.24
;; ============================================================================

;; Paper

;; ============================================================================
;; Exercise 2.25
;; ============================================================================

;; (define l1 (list 1 3 (list 5 7) 9))
;; (car (cdr (car (cdr (cdr l1))))) => (car (cdaddr l1))

;; (define l2 (list (list 7)))
;; (caar l2)

;; (define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;; (cadadr (cadadr (cadadr l3)))

;; ============================================================================
;; Exercise 2.26
;; ============================================================================

;; (define x (list 1 2 3))
;; (define y (list 4 5 6))

;; (append x y) => (1 2 3 4 5 6)
;; (cons x y) => ((1 2 3) 4 5 6)
;; (list x y) => ((1 2 3) (4 5 6))

;; ============================================================================
;; Exercise 2.27
;; ============================================================================

(define x (list (list 1 2) (list 3 4) 7 (list 5 9 20)))

;; (define (deep-reverse t)
;;   (cond ((null? t) t)

;;         ((pair? (car t)) (append (deep-reverse (cdr t))
;;                                  (list (deep-reverse (car t)))))

;;         (else (append (deep-reverse (cdr t))
;;                       (list (car t))))))

;; (deep-reverse x)

;; ============================================================================
;; Exercise 2.28
;; ============================================================================

(define x (list (list 1 2) (list 3 4)))

(define (fringe t)
  (cond ((null? t) t)
        ((list? (car t)) (append (fringe (car t)) (fringe (cdr t))))
        (else (cons (car t) (fringe (cdr t))))))

;;(fringe x)

;; ============================================================================
;; Exercise 2.29
;; ============================================================================

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a) Write the corresponding selectors `left-branch' and `right-branch', which
;; return the branches of a mobile, and `branch-length` and `branch-structure`,
;; which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b) Using your selectors, define a procedure `total-weight` that returns the
;; total weight of a mobile.

(define (total-weight mobile)
  ;; There's a left branch and a right branch.
  (cond ((number? mobile) mobile)
        ((null? mobile)   0)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;; c) A mobile is said to be balanced if the torque applied by its top-left
;; branch is equal to that applied by its top-right branch (that is, if the
;; length of the left rod multiplied by the weight hanging from that rod is
;; equal to the corresponding product for the right side) and if each of the
;; submobiles hanging off its branches is balanced. Design a predicate that
;; tests whether a binary mobile is balanced.

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile))
        (left-structure (branch-structure (left-branch mobile)))
        (right-structure (branch-structure (right-branch mobile))))

    ;; If the branch structure of both branches is a number, all we need to
    ;; compare are their torques.
    (cond ((and (number? left-structure) (number? right-structure))
           (= (torque left) (torque right)))

          ;; If both of the structures are lists, they must both be mobiles;
          ;; compare the torque of left and right branches and check that the
          ;; submobiles are balanced.
          ((and (list? left-structure) (list? right-structure))
           (and (= (torque left) (torque right))
                (balanced-mobile? left-structure)
                (balanced-mobile? right-structure)))

          ;; If one branch structure is a weight and one is a mobile, they
          ;; cannot be balanced.
          (else #f))))

(define balanced-mobile-data
  (make-mobile
   (make-branch 1 (make-mobile
                   (make-branch 1 3)
                   (make-branch 0 0)))
   (make-branch 3 (make-mobile
                   (make-branch 1 3)
                   (make-branch 1 1)))))

;; (balanced-mobile? balanced-mobile-data)

;; d. Not much! Just need to change the `cadr's to `cdr's.

;; ============================================================================
;; Exercise 2.30
;; ============================================================================

(define (square x) (* x x))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree-direct tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree-direct (car tree))
                            (square-tree-direct (cdr tree))))
        (else (square tree))))

;; ============================================================================
;; Exercise 2.31
;; ============================================================================

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

;; given

(define (square-tree tree) (tree-map square tree))

(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

;; ============================================================================
;; Exercise 2.32
;; ============================================================================

;; TODO: This one.

;; (define (subsets s)
;;   (if (null? s)
;;       '()
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map ?? rest)))))

;; (subsets (list 1 2 3))

;; ============================================================================
;; Exercise 2.33
;; ============================================================================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (r-map p sequence)
  (accumulate (lambda (next acc) (cons (p next) acc)) '() sequence))

(r-map (lambda (x) (+ x 1)) (list 0 1 2 3))

(define (r-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (r-length sequence)
  (accumulate (lambda (next acc) (+ 1 acc)) 0 sequence))

;; ============================================================================
;; TODO: Exercise 2.34
;; ============================================================================

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (if (= this-coeff 0)
                    higher-terms
                    (+ (* this-coeff x) higher-terms)))
              0
              coefficient-sequence))

;; (horner-eval 2 (list 2 0 0))

;; (horner-eval 2 (list 1))

;; (* x 1)

;; ============================================================================
;; Exercise 2.35
;; ============================================================================

;; Strategy: Transform the input tree into a list of the number of leaves in
;; each subtree and accumulate the result w/ addition

(define (count-leaves t)
  (accumulate
   (lambda (next acc)
     (+ next acc))
   0
   (map (lambda (sub-t)
          (if (not (pair? sub-t))
              1
              (count-leaves sub-t)))
        t)))

(define count-leaves-tree (cons (list 1 2) (list 3 4)))

(count-leaves (list count-leaves-tree count-leaves-tree))


;; ============================================================================
;; Exercise 2.36
;; ============================================================================

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; ============================================================================
;; Exercise 2.37
;; ============================================================================

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r)
         (dot-product r v))
       m))

(define matrix (list (list 1 2 3 4)
                     (list 4 5 6 6)
                     (list 6 7 8 9)))

(define transposed (list (list 1 4 6)
                         (list 2 5 7)
                         (list 3 6 8)
                         (list 4 6 9)))

(dot-product (list 1 2 3 4) (list 1 2 3 4))
(matrix-*-vector matrix (list 1 2 3 4))

(define (transpose m)
  (accumulate-n cons
                '()
                m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

(matrix-*-matrix matrix transposed)


;; ============================================================================
;; Exercise 2.38
;; ============================================================================

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))

(fold-right / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(fold-right list '() (list 1 2 3))

(fold-left list '() (list 1 2 3))

;; op is called with `acc' and `next', and the ordering of those parameters is
;; different depending on whether we are folding left or right. `op' must
;; produce the same output regardless of the ordering of its parameters in order
;; for `fold-left` and `fold-right' to produce the same values.

;; ============================================================================
;; Exercise 2.39
;; ============================================================================

(define (reverse-foldr seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))

(define (reverse-foldl seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(reverse-foldr (list 1 2 3 4))
(reverse-foldl (list 1 2 3 4))

;; ============================================================================
;; Notes
;; ============================================================================

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ start 1) end))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 6))

(define (remove x xs)
  (filter (lambda (y) (not (= x y))) xs))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;; (permutations (list 1 2 3))


;; ============================================================================
;; Exercise 2.40
;; ============================================================================

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;; ============================================================================
;; Exercise 2.41
;; ============================================================================

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; TODO: How about a "unique-tuple" that took a size "s" and num "n"?

(define (unique-triple-sum n s)
  (filter (lambda (triple)
            (= s (+ (car triple) (cadr triple) (caddr triple))))
          (unique-triples n)))

;; ============================================================================
;; Exercise 2.42
;; ============================================================================

(define empty-board
  (list))

;; *new-row* is an integer from 1 to board-size
;; *k* is the column we're on, also an integer
;; *rest-of-queens* is "a way to place k-1 queens in the first k-1 columns"
;; 
;; RESULT: Adjoins a new row-column position to a set of positions
(define (adjoin-position new-row k rest-of-queens)
  )

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(enumerate-interval 1 8)
