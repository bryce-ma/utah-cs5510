#lang plai-typed

;; Part 0 — Create Handin Account
;; omitted

;; Part 1 — Implement 3rd-power
(define 3rd-power
  (lambda (x)
    (* x (* x x))))

(3rd-power 17)

;; Part 2 — Implement 42nd-power
(define 42nd-power
  (lambda (x)
    (let* ([3rd (3rd-power x)]
          [9th (3rd-power 3rd)])
      (* (/ (3rd-power 9th) 3rd) ; power 24
         (* 9th 9th))))) ; power 18

(42nd-power 17)

;; Part 3 — Implement plural
(define plural
  (lambda (x)
    (let* ([last-index (- (string-length x) 1)]
          [last (string-ref x last-index)])
      (if (equal? last #\y)
          (string-append (substring x 0 last-index)
                         "ies")
          (string-append x "s")))))

(plural "baby")
(plural "fish")

;; Part 4 — Implement energy-usage
(define-type Light
    [bulb (watts : number)
          (technology : symbol)]
    [candle (inches : number)])
(define energy-usage
  (lambda (x)
    (type-case Light x
      (bulb (w t) (/ (* 24 w) 1000))
      (candle (i) 0.0))))

(energy-usage (bulb 100.0 'halogen))
(energy-usage (candle 10.0))

;; Part 5 — Implement use-for-one-hour
(define use-for-one-hour
  (lambda (x)
    (type-case Light x
      (bulb (w t) (bulb w t))
      (candle (i) (candle (if (< i 1)
                              0
                              (- i 1)))))))

(use-for-one-hour (bulb 100.0 'halogen))
(use-for-one-hour (candle 10.0))

;; Part 6 — Handin
;; omitted