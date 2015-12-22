;; ===============================================================
;; Huffman Trees
;; ===============================================================

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; ===============================================================
;; Exercise 2.67
;; ===============================================================

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; "ADABBCA"

;; ===============================================================
;; Exercise 2.68
;; ===============================================================

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol))
         '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol: ENCODE-SYMBOL" symbol))))

;; (encode (decode sample-message sample-tree) sample-tree)

;; ===============================================================
;; Exercise 2.69
;; ===============================================================

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (insert x xs)
    (cond ((null? xs) (list x))
          ((< (weight x) (weight (car xs))) (cons x xs))
          (else (cons (car xs) (insert x (cdr xs))))))

  (cond ((null? leaf-set)
         (error "empty leaf-set: SUCCESSIVE-MERGE"))
        ((= 1 (length leaf-set)) (car leaf-set))
        (else
         (let ((merged (make-code-tree (car leaf-set)
                                       (cadr leaf-set))))
           (successive-merge (insert merged (cddr leaf-set)))))))

(define sample-leaf-set
  (make-leaf-set '((A 4)  (C 1) (B 2) (D 1))))

;; (successive-merge sample-leaf-set)

;; ===============================================================
;; Exercise 2.70
;; ===============================================================

(define rock-songs-tree
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1)
                           (BOOM 1) (JOB 2) (NA 16) (YIP 9))))


;; (length
;;  (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
;;          rock-songs-tree))

;; Only 84 bits required!

;; Would need three bits per symbol, at 36 symbols.
;; (* 3 36) ;; => 108

;; ===============================================================
;; Exercise 2.71
;; ===============================================================

;; TODO: sketch...

;; ===============================================================
;; Exercise 2.72
;; ===============================================================

;; TODO: ugh math
