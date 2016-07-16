#lang plai-typed

(define-type Tree
  [leaf (val : number)]
  [node (val : number)
        (left : Tree)
        (right : Tree)])

;; Part 1 — Sum
(define sum
  (lambda (x)
    (type-case Tree x
      [node (v l r) (+ v (+ (sum l) (sum r)))]
      [leaf (v) v])))

(test (sum (node 5 (leaf 6) (leaf 7))) 18)

;; Part 2 — Negate
(define negate
  (lambda (x)
    (type-case Tree x
      [node (v l r) (node (- 0 v) (negate l) (negate r))]
      [leaf (v) (leaf (- 0 v))])))

(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))

;; Part 3 — Contains?
(define contains?
  (lambda (x value)
    (type-case Tree x
      [node (v l r) (if (= v value)
                        #t
                        (or (contains? l value) (contains? r value)))]
      [leaf (v) (= v value)])))

(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)

;; Part 4 — Big Leaves?
(define big-leaves?
  (lambda (x)
    (type-case Tree x
      [node (v l r) (and (type-case Tree l
                          [node (vv ll rr) (big-leaves? (node (+ v vv) ll rr))]
                          [leaf (lv) (> lv v)])
                        (type-case Tree r
                      [node (vv ll rr) (big-leaves? (node (+ v vv) ll rr))]
                      [leaf (lv) (> lv v)]))]
      [leaf (v) (> v 0)]))) ;; this line will never be reached 

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)

;; Part 5 — Optional challenge: Sorted? in-order traversal (中序遍历)
(define sorted?
  (lambda (x)
    ()))