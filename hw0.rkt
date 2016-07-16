#lang plai-typed

;; Part 0 — Create Handin Account
;; omitted

;; Part 1 — Implement 3rd-power
(define 3rd-power
  (lambda (x)
    (* x (* x x))))

(3rd-power 17)

(define 42nd-power
  (lambda (x)
    (let* ([3rd (3rd-power x)]
          [9th (3rd-power 3rd)])
      (* (/ (3rd-power 9th) 3rd) ; power 24
         (* 9th 9th))))) ; power 18

(42nd-power 17)

()