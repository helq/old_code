(/ 1000 334)
(/ 1000.0 334)
(define n 2)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define (square x) (* x x))
(define (^ x) (* x x))

(define (+ x y) (* x y))


(define (abs x)
  (cond ((>= x 0) x)
        ((< x 0) (- x))
        )
  )

(define (abs x)
  (if (< x 0)
      (- x)
      x
  )
)

; excercise 1.2
(/
    (+  5
        4
        (-  2
            (-  3
                (+ 6 (/ 4 5))
            )))
    (*  3
        (- 6 2)
        (- 2 7))
)

; excercise 1.3
(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)) )

(define (proc1 a b c) (cond ((and (< a b) (< a c)) (sum-squares b c))
                            ((and (< a b) (< c a)) (sum-squares b a))
                            (else (sum-squares a c))
                            ))

(proc1 1 2 3) ; 13
(proc1 3 2 3) ; 13

; excercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 5 4) ; 9
(a-plus-abs-b 5 -4) ; 9
(a-plus-abs-b -5 4) ; -1

; excercise 1.5
(define (p) (p)) ; inf loop

(define (test x y)
  (if (= x 0)
      0
      y))


; section 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs 
         (- (/ (square guess) x)
            1.0))
     0.001))

(define (sqrt x)
  (sqrt-iter (/ x 2) x))

(sqrt 100.0)
(sqrt 2.0)

; 1.8
(define (cube x) (* x x x))

(define (cube-root-iter guess x)
  (if (good-enough3? guess x)
      guess
      (cube-root-iter (improve3 guess x)
                 x)))

(define (improve3 guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (good-enough3? guess x)
  (< (abs 
         (- (/ (cube guess) x)
            1.0))
     0.00001))

(define (cube-root x)
  (cube-root-iter (/ x 2) x))

(cube-root 1000.0)
(cube-root 8.0)


; ex 1.1.8
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; Excercise 1.10: Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ; 2^10 = 1024

(A 2 4) ; 2^2^2^2 = 65536

(A 3 3) ; 2^(2^2^2) = 65536

(define (f n) (A 0 n)) ; == 0

(define (g n) (A 1 n)) ; == 2^n

(define (h n) (A 2 n)) ; == 2^2^.. (n times in total) ..^2^2


; Excercise 1.16
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 2 50); == 1125899906842624

; by helq
(define (^ b n)
    (define (loop a n)
        (cond ((= n 1) a)
              ((even? n) (loop (* a a) (/ n 2)))
              ( else     (loop (* a b) (- n 1)))
              ))
    (if (= n 0) 1 (loop b n)) )

(^ 2 50); == 1125899906842624

; by http://eli.thegreenplace.net/2007/07/04/sicp-sections-124-125/
(defun fast-expt-iter (b n &optional (a 1))
  (cond ((= n 0) a)
        ((evenp n) (fast-expt-iter (square b) (/ n 2) a))
        (t (fast-expt-iter b (- n 1) (* b a)))))

; Excercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))   ; compute p'
                   (+ (* 2 p q) (* q q)) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.2.6 Example: Testing for Primality

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Excercise: 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(lambda (x) (* x x))


; Excercise: 1.27

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael_number? n)
    (define (loop a)
        (cond ((= a n) true)
              ((not (= a (expmod a n n))) false)
              (else (loop (+ a 1))) ))
    (and (not (prime? n)) (loop 2))
    )

(define (test_carmichaels_numbers start end)
    (cond 
       ((> start end) (newline) (display "end"))
       (else (cond ((carmichael_number? start)
                       (newline)
                       (display start)
                       (display " *** ")))
             (test_carmichaels_numbers (+ start 1) end))
        ))

(test_carmichaels_numbers 2 100000)


; section 1.3.1

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


; excercise 1.29

(define (succ x) (+ x 1))

(define (simpsons_rule f a b n)
    (define h (/ (- b a) n))
    (define (term k) (* (if (even? k) 2 4)
                        (f (+ a (* k h)))) )
    (* (/ h 3) (+ (f a) (sum term 1 succ (- n 1)) (f b)))
    )

(simpsons_rule (lambda (n) (* n n n)) 0 1 10) ; 1/4

(simpsons_rule (lambda (n) (* n n)) 1 1 10) ; 0

(define pi 3.14159265358979323846)

(simpsons_rule (lambda (n) (sin n)) 0 (/ pi 2) 10)   ; 1.0000033922209004

(simpsons_rule (lambda (n) (sin n)) 0 (/ pi 2) 1000) ; 1.0000000000000333

; excercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


; excercise 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 1))


; excercise 1.32

(define (accumulate combiner null-value term a next b) ; foldl
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b) ; foldr
  (define (accu a b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accu (next a) b))))
  (accu a b))

(define (product term a next b) (accumulate * 1 term a next b))

(define (sum term a next b) (accumulate + 0 term a next b))

(sum (lambda (n) n) 1 succ 10)

; excercise 1.33

(define (filtered-accumulate combiner null-value condition term a next b) ; foldl . filter
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (condition a) (combiner (term a) result)
                                result))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value condition term a next b) ; foldr . filter
  (define (accu a b)
    (if (> a b)
        null-value
        (if (condition a)
            (combiner (term a) (accu (next a) b))
            (accu (next a) b))))
  (accu a b))

(define (identity x) x)

(define (sum term a next b)
    (filtered-accumulate + 0 (lambda (x) true) term a next b))

(sum (lambda (n) n) 1 succ 10)

; section 1.3.3
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


; excercise 1.35

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))


; excercise 1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)


; excercise 1.37

; recursive
(define (cont-frac n d k)
    (define (iter i)
        (if (> i k)
            0
            (/ (n i)
               (+ (d i) (iter (+ 1 i))))))
    (iter 1))

; iterative
(define (cont-frac n d k)
    (define (iter i accu)
        (if (< i 1)
            accu
            (iter (- i 1)
                  (/ (n i)
                     (+ (d i) accu)))))
    (iter k 0))


(/ 1 (cont-frac (lambda _ 1.0) (lambda _ 1.0) 100))


; excercise 1.37

(define (d_euler i)
    (if (= 0 (modulo (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1))

; testing
(map d_euler (iota 11 1))

(+ 2 (cont-frac (lambda _ 1.0) d_euler 21 ))


; excercise 1.38

(define (tan-cf x k)
    (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
               (lambda (i) (- (* 2 i) 1))
               k))

(tan-cf 3.14159265358979 1000) ; tan(pi) == 0
(tan-cf (/ 3.14159265358979 2) 1000) ; tan(pi/2) == inf


; excercise 1.42

(define (compose f g)
    (lambda (x) (f (g x))))

((compose (lambda (n) (* 5 n)) (lambda (n) (+ n 2))) 3) ; 25

; excercise 1.43

(define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (lambda (x) (f ((repeated f (- n 1)) x)))))

((repeated (lambda (n) (* 5 n)) 3) 3) ; 375

(define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated f (- n 1)) ))))

; excercise 1.44

(define (smooth f)
    (let ((dx 0.00001))
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3))))

((smooth (lambda (n) (* n n))) 2)

(define (n-fold-smooth f n)
    ((repeated smooth n) f))

((n-fold-smooth (lambda (n) (* n n n)) 100) 2)


; excercise 1.46

(define (average x y) (/ (+ x y) 2))

(define (iterative-improve guess-enough? improve)
    (lambda (first-guess)
        (define (try guess)
          (let ((next (improve guess)))
            (if (guess-enough? guess next)
                next
                (try next))))
        (try first-guess)))

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (sqrt x)
    ((iterative-improve
        close-enough?
        (lambda (y) (average y (/ x y))))
    1.0))

(sqrt 4)

(define (fixed-point f first-guess)
    ((iterative-improve close-enough? f) first-guess))


; Section 2.1.1

(define (make-rat n d)
    (let ((g (gcd n d)))
         (cons (/ n g) (/ d g))))
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; excercise 2.1

(define (make-rat n d)
    (let ((g (gcd n d))
          (sign (if (< d 0) -1 1)))
         (if (= d 0)
             (error "denominator cannot be zero")
             (cons (* (/ n g) sign) (* (/ d g) sign)))))


; excercise 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment s f)
        (cons s f))

(define (start-segment seg)
        (car seg))

(define (finish-segment seg)
        (cdr seg))

(define (midpoint-segment seg)
        (let ((s (start-segment seg))
              (f (finish-segment seg)))
             (make-point (average (x-point s) (x-point f))
                         (average (y-point s) (y-point f)))))

(define (make-point x y)
        (cons x y))

(define x-point car)
(define y-point cdr)

(define (average x y) (/ (+ x y) 2))

(print-point (midpoint-segment (make-segment (make-point 2 5) (make-point 1 8))))


; excercise 2.3

(define (on comp in)
    (lambda (x y) (comp (in x)
                        (in y))))

(define (make-rectangle p1 p2)
    (let ((is-rectangle? (not (and ((on = x-point) p1 p2)
                                   ((on = y-point) p1 p2))))
          (points-in-order? ((on < x-point) p1 p2)))

    (cond ((not is-rectangle?) (error "doesn't a rectangle"))
          (points-in-order?    (cons p1 p2))
          (else  (let ((new-p1 (make-point (x-point p1)
                                           (y-point p2)))
                       (new-p2 (make-point (x-point p2)
                                           (y-point p1))))
                      (if ((on < x-point) new-p1 new-p2)
                          (cons new-p1 new-p2)
                          (cons new-p2 new-p1))))))))

(make-rectangle (make-point 1 2) (make-point 5 6))

(make-rectangle (make-point 7 2) (make-point 5 6))


(define point-left-rectangle car)
(define point-right-rectangle cdr)

(define (perimeter r)
    (let ((pl (point-left-rectangle r))
          (pr (point-right-rectangle r)))
         (+ (* 2 ((on - x-point) pr pl))
            (* 2 ((on - y-point) pr pl)))))

(perimeter (make-rectangle (make-point 7 2) (make-point 5 6))) ; 12

(define (area r)
    (let ((pl (point-left-rectangle r))
          (pr (point-right-rectangle r)))
         (* ((on - x-point) pr pl)
            ((on - y-point) pr pl))))

(area (make-rectangle (make-point 7 2) (make-point 5 6))) ; 8


; excercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


; excercise 2.5

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (what-power? n p)
    (define (loop m accu)
        (if (= (modulo m p) 0)
            (loop (/ m p) (+ accu 1))
            accu))
    (loop n 0))

(what-power? 81 3) ; 4

(define (car z)
  (what-power? z 2))

(define (cdr z)
  (what-power? z 3))

(car (cons 15 3)) ; 15

(cdr (cons 15 3)) ; 13


; excercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f)
                (lambda (x) (f x))))

(define two (lambda (f)
                (lambda (x) (f (f x)))))

(define (test num)
    ((num (lambda (l) (+ 1 l))) 0))

(define (add m n)
    (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(test (add two one)) ; 3


; section 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

; excercise 2.7

(define (make-interval a b) (cons a b))

(define upper-bound cdr)

(define lower-bound car)

; excercise 2.8

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))


; excercise 2.10

(define (div-interval x y)
  (let ((upper (upper-bound y))
        (lower (lower-bound y)))
  (if (or (= upper 0) (= lower 0))
      (error "the interval y might be zero")
      (mul-interval x 
                    (make-interval (/ 1.0 upper)
                                   (/ 1.0 lower))))))



; excercise 2.17

(define (last-pair items)
    (if (null? (cdr items))
        items
        (last-pair (cdr items))))

(last-pair (list 23 72 149 34)) ; (34)


; excercise 2.18

(define (reverse items)
    (if (null? items)
        '()
        (append (reverse (cdr items))
                (list (car items)))))

(define (reverse items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (car things) answer))))
    (iter items '()))


(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)


; excercise 2.19

(define (cc total coins)
    (cond ((null? coins) 0)
          ((< total 0) 0)
          ((= total 0) 1)
          (else (+ (cc (- total (car coins)) coins)
                   (cc total (cdr coins))))))

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins) ; 292

; excercise 2.20

(define (same-parity first . tail)
    (define (parity n) (modulo n 2))
    (define parity-of-first (parity first))
    (cons first
          (filter (lambda (n) (= (parity n) parity-of-first))
                  tail)))

(same-parity 1 2 3 4 5 6 7); (1 3 5 7)

(same-parity 2 3 4 5 6 7); (2 4 6)


; generalize map

(define (map f ls . lss)
    
    (define (map-in-one-list g ls)
        (if (null? ls)
            '()
            (cons (g (car ls))
                (map-in-one-list g (cdr ls)))))

    (define (or-list ls)
        (cond ((null? ls) false)
              ((car ls)   true)
              (else       (or-list (cdr ls)))))

    (define (null-any-list? lss)
            (or-list (map-in-one-list null? lss)))
    
    (define (map-help lss)
            (if (null-any-list? lss)
            '()
            (cons (apply  f (map-in-one-list car lss))
                  (map-help (map-in-one-list cdr lss)))))
    
    (map-help (cons ls lss))))

;testing
(map + (list 1 2) (list 7 3 6)) ; (8 5)


; excercise 2.23

; see very bad in kate
;; (define (for-each f ls)
;;     (map f ls)
;;     #!unspecific)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


; excercise 2.25

(list 1 3 (list 5 7) 9)

(define x (cons 1 (cons 3 (cons (cons 5 (cons 7 '())) (cons 9 '())))))

(car (cdr (car (cddr x))))


(define y (cons (cons 7 '()) '()))

(caar y)


(define z (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 '())) '())) '())) '())) '())) '())))

(cadadr (cadadr (cadadr z)))


; excercise 2.27

(define (deep-reverse items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (if (pair? (car things))
                            (iter (car things ) '())
                            (car things))
                        answer))))
    (iter items '()))

;test
(deep-reverse (list (list 1 2) (list 3 4))) ; ((4 3) (2 1))


; excercise 2.28

(define (fringe tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree))
                        (fringe (cdr tree))))))

(fringe (list (list 1 2) (list 3 4))) ; (1 2 3 4)


; excercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


(define left-branch car)

(define right-branch cadr)

(define branch-length car)

(define branch-structure cadr)


(define (total-weight mobile)
    (if (not (pair? mobile)) ; then is a weight
        mobile
        (let ((left  (left-branch  mobile))
              (right (right-branch mobile)))
          (+ (total-weight (branch-structure left))
             (total-weight (branch-structure right))))))

(define (torque branch)
    (* (branch-length branch)
       (weight (branch-structure branch))))

(define (balanced? mobile)
    (let ((left  (left-branch  mobile))
          (right (right-branch mobile)))
         (and (= (torque left)
                 (torque right))
              (balanced? (branch-structure left))
              (balanced? (branch-structure right)))))


(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; redefining

(define left-branch car)

(define right-branch cdr)

(define branch-length car)

(define branch-structure cdr)


; map-tree

(define (map-tree f tree)
    (map (lambda (subtree)
            (if (pair? subtree)
                (map-tree f subtree)
                (f subtree)))
         tree))


; excercise 2.31

(define (square-tree tree)
    (map-tree (lambda (n) (* n n))
              tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; (1 (4 (9 16) 25) (36 49))


; excercise 2.32

(define (subsets s)
    (if (null? s)
        (list '())
        (let ((s1 (subsets (cdr s)))
              (e  (car s)))
             (append s1
                     (map (lambda (e1) (cons e e1)) s1)))))


; excercise 2.33

(define (map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (fold-right cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


; excercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                      (+ this-coeff
                         (* x higher-terms)))
              0
              coefficient-sequence))

; excercise 2.35

(define (count-leaves t)
  (fold-right +
        0
        (map (lambda (subtree)
                     (if (pair? subtree)
                         (count-leaves subtree)
                         1))
             t)))

; excercise 2.36

(define (fold-n f initial seqs)
    (if (null? (car seqs))
        '()
        (cons (fold-right f initial (map car seqs))
          (fold-n f initial (map cdr seqs)))))

; excercise 2.37

(define (dot-product v w)
  (fold-right + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (w) (dot-product v w)) m))

(define (matrix-*-matrix m n)
    (let ((nT (transpose n)))
         (map (lambda (v) (matrix-*-vector nT v))
              m)))

(define (transpose m)
    (fold-n cons '() m))


; excercise 2.38

(define (fold-left f i ls)
    (if (null? ls)
        i
        (fold-left f (f i (car ls)) (cdr ls))))


; excercise 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))


; section 2.2.3

(define (flat seq)
  (fold-right append '() seq))

(define (flatmap f seq)
    (flat (map f seq)))


; excercise 2.42

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? board-size positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row board-size rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (enumerate-interval n m)
    (iota (+ (- m n) 1) n))

(define (adjoin-position new-row k rest-of-queens)
    (cons (map (lambda (n) (= new-row n))
               (enumerate-interval 1 k))
          rest-of-queens))

(define (or-list ls)
    (cond ((null? ls) false)
          ((car ls)   true)
          (else       (or-list (cdr ls)))))

(define (safe? k positions)
    (define inter-1-k (enumerate-interval 1 k))
    
    (define (find-pos ls)
        (cdar (filter car
                     (map cons ls inter-1-k))))
    
    (define pos (find-pos (car positions)))

    (define positions-attack-of-first
      (map (lambda (i)
                   (map (lambda (m)
                                (or (= m pos)
                                    (= m (- pos i))
                                    (= m (+ pos i))))
                        inter-1-k))
           (enumerate-interval 1(length (cdr positions)))))
    
    (not (or-list (map (lambda (x y) (and x y))
                       (flat positions-attack-of-first)
                       (flat (cdr positions))))))



; excercise 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                          (base exp)
                                          (- (exponent exp) 1)))
                        (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (make-exponentiation b e)
    (cond ((=number? b 1) 1)
          ((=number? b 0) (error "exponent cannot be zero"))
          ((not (number? e)) (error "only number as exponents"))
          ((=number? e 0) 1)
          ((=number? e 1) b)
          ((number? b) (expt b e))
          (else (list '** b e))))

(define (exponentiation? e)
    (and (pair? e) (eq? (car e) '**)))

(define base cadr)

(define exponent caddr)

;test
(deriv '(** x 4) 'x) ; (* 4 (** x 3))

; exponent 2.57

; BUGS everywere, only here XD

;; (define (summands s) (cdr s)) ; return a list of summands
;; 
;; (define (multiplicands p) (cdr p)) ; return a list of multiplicands
;; 
;; (define (all p xs)
;;     (cond ((null? xs) true)
;;           ((not (p (car xs))) false)
;;           (else (all p (cdr xs)))))
;; 
;; (define (make-sum a)
;;   (cond ((= (length a) 0) 0)
;;         ((all number? a) (apply + a))
;;         (else (cons '+
;;                     (filter (lambda (n) (not (=number? n 0)))
;;                             a)))))
;; 
;; (define (=number? exp num)
;;   (and (number? exp) (= exp num)))
;; 
;; (define (make-product ms)
;;   (cond ((= 0 (length ms)) 1)
;;         ((any (lambda (n) (=number? n 0)) ms) 0)
;;         ((all number? ms) (apply * ms))
;;         (else (cons '* (filter (lambda (n) (not (=number? n 1)))
;;                                ms)))))
;; 
;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (map (lambda (s) (deriv s var))
;;                         (summands exp))))
;;         ((product? exp)
;;          (let ((multiplier   (car (multiplicands exp)))
;;                (multiplicand (cdr (multiplicands exp))))
;;               (make-sum
;;                  (list
;;                      (make-product (cons multiplier
;;                                      (let ((multiplic1 (deriv (make-product multiplicand)
;;                                                              var)))
;;                                           (if (pair? multiplic1)
;;                                               multiplic1
;;                                               (list multiplic1)))))
;;                       (make-product (cons (deriv multiplier var)
;;                                           multiplicand))))))
;;         ((exponentiation? exp)
;;          (make-product (list (exponent exp)
;;                              (make-exponentiation
;;                                           (base exp)
;;                                           (- (exponent exp) 1))
;;                              (deriv (base exp) var))))
;;         (else
;;          (error "unknown expression type -- DERIV" exp))))



; excercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; excercise 2.60

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  
  (define (element-of-set?andDelete x set)
      (cond ((null? set) (cons false '()))
            ((equal? x (car set)) (cons true (cdr set)))
            (else (define new-set-and-boolean
                          (element-of-set?andDelete x (cdr set)))
                  (let ((new-set (cdr new-set-and-boolean))
                        (elem-in-set? (car new-set-and-boolean)))
                       (if boolean
                           (cons true (cons (car set) new-set))
                           (cons false '()))))))
  
  (define new-set2-and-boolean
          (element-of-set?andDelete (car set1) set2))
  (define element-in-set2? (car new-set2-and-boolean))
  (define new-set2 (cdr new-set2-and-boolean))
  
  (cond ((or (null? set1) (null? set2)) '())
        ((element-in-set2? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) new-set2)))
        (else (intersection-set (cdr set1) set2))))


; excercise 2.61

(define (adjoin-set x set)
  (let ((x1 (car set)))
       (cond ((null? set) (list x))
             ((= x1 x) set)
             ((< x1 x) (cons x1 (adjoin-set x (cdr set))))
             (else (cons x set)))))

; excercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (y1 (car set2))
                    (new-set1 (cdr set1))
                    (new-set2 (cdr set2)))
                   (cond ((= x1 y1) (cons x1 (union-set new-set1 new-set2)))
                         ((< x1 y1) (cons x1 (union-set new-set1 set2)))
                         (else      (cons y1 (union-set set1 new-set2))))))))


; excercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (let ((symbols-left  (symbols (left-branch tree)))
          (symbols-right (symbols (right-branch tree)))
          (left-tree  (left-branch tree))
          (right-tree (right-branch tree)))
         (cond ((element-of-set? symbol symbols-left)
                    (if (leaf? left-tree)
                        '(0)
                        (cons 0 (encode-symbol symbol left-tree))))
               ((element-of-set? symbol symbols-right)
                    (if (leaf? right-tree)
                        '(1)
                        (cons 1 (encode-symbol symbol right-tree))))
               (else (error "symbol " symbol " don't is part of the tree")))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        ((eq? x (car set)) false)
        (else (element-of-set? x (cdr set)))))


; excercise 2.69

'((A 4) (B 2) (C 1) (D 1))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
    (cond ((null? trees) '())
          ((null? (cdr trees)) (car trees))
          (else (successive-merge (adjoin-set (make-code-tree (car trees)
                                                             (cadr trees))
                                             (cddr trees))))))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))


; excercise 2.70

(define huffman-tree-of-lirycs
   (generate-huffman-tree
       '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define message '(Get a job
                  Sha na na na na na na na na
                  Get a job
                  Sha na na na na na na na na
                  Wah yip yip yip yip yip yip yip yip yip
                  Sha boom))

(define encoded-message (encode message huffman-tree-of-lirycs))
;=> (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(define decoded-message (decode encoded-message huffman-tree-of-lirycs))
;=> (get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom)

(length encoded-message)
; vs
(* 3 (length message)) ; 3 bit per symbol in fixed length


; excercise 2.71
; most frecuent character is represented with one bit
; lest frecuent character is represented with n-1 bits


; excercise 2.72

(define (install-derive-package)
  ;; internal procedures
  (define (deriv-+ summands var)
      (make-sum (deriv (addend summands) var)
                (deriv (augend summands) var)))
  (define (deriv-* multiplicands var)
      (make-sum
           (make-product (multiplier multiplicands)
                         (deriv (multiplicand multiplicands) var))
           (make-product (deriv (multiplier multiplicands) var)
                         (multiplicand multiplicands))))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-+)
  (put 'deriv '* deriv-*)
  'done)



; excercise 3.1

(define (make-accumulator accu)
    (lambda (toAdd)
        (set! accu (+ toAdd accu))
        accu))

;; (define A (make-accumulator 5))
;; (A 10)
;; 15
;; (A 10)
;; 25

; excercise 3.2

(define (make-monitored procedure)
    (define total-calls 0)
    (define (dispatch args)
        (cond ((eq? args 'how-many-calls?) total-calls)
              (else (set! total-calls (+ 1 total-calls))
                    (procedure args))))
    dispatch)

;; (define s (make-monitored sqrt))
;; 
;; (s 100)
;; 10
;; 
;; (s 'how-many-calls?)
;; 1

; excercise 3.3

(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass secret-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

;; (define acc (make-account 100 'secret-password))
;; 
;; ((acc 'secret-password 'withdraw) 40)
;; 60
;; 
;; ((acc 'some-other-password 'deposit) 50)
;; "Incorrect password"


; excercise 3.4

(define (make-account balance secret-password)
  (define consecutive-times-bad-password 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass secret-password)
        (begin (set! consecutive-times-bad-password 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))
        (begin (if (= consecutive-times-bad-password 7)
                   (call-the-cops)
                   (set! consecutive-times-bad-password
                         (+ 1 consecutive-times-bad-password)))
               (error "Incorrect password"))))
  dispatch)


; excercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
    (* (monte-carlo trials
                   (lambda _ (P (random-in-range x1 x2)
                                (random-in-range y1 y2))))
       (- x2 x1)
       (- y2 y1)))

(define (unit-circle x y)
    (<= (+ (square x) (square y)) 1))

(define pi (estimate-integral unit-circle -1.0 1.0 -1.0 1.0 100000))


; excercise 3.7

(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (change-password new-password)
      (set! secret-password new-password))
  (define (dispatch pass m)
    (if (eq? pass secret-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'change-password) change-password)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

(define (make-joint account password new-password)
    ((account password 'change-password) new-password)
    account)


; excercise 3.8

(define m 0)

(define (f n)
    (define toRet
        (let ((temp (- n m)))
             (if (< temp 0)
                 0
                 temp)))
    (set! m (+ m 1))
    toRet)


; excercise 3.17

(define (count-pairs xs)
    (define (not-visited?         xs visited)
            (not (element-of-set? xs visited))) ;; using eq? as comparator
    (define add-visit adjoin-set)
    
    (define (walk xs visited)
        (if (not (pair? xs))
            visited
            (if (not-visited? xs visited)
                 (let ((new-visited (walk (car xs) (add-visit xs visited))))
                      (walk (cdr xs) new-visited))
                 visited)))
    
    ; more compact
    (define (walk xs visited)
        (if (or (not (pair? xs)) (visited? xs visited))
            visited
            (let ((new-visited (walk (car xs)(add-visit xs visited))))
                 (walk (cdr xs) new-visited))))
    
    (walk xs empty-set))


; excercise 3.18

; bad solution
(define (is-list-cycle? xs)
    (define (loop xs1)
        (cond ((null? xs1) false)
              ((eq? xs xs1) true)
              (else (loop (cdr xs1)))))
    (if (null? xs)
        false
        (loop (cdr xs))))


; section 3.3.2

; my solution -- object oriented
; o_o oh is excercise 3.22
(define (make-queue)
    (define q (cons '() '()))
    (define r q)
    (define (empty-queue?) (eq? q r))
    (define (front-queue)
        (if (empty-queue?)
            (error "queue empty")
            (cadr q)))
    (define (insert! i)
        (set-cdr! r (list i))
        (set! r (cdr r)))
    (define (remove!)
        (if (empty-queue?)
            (error "queue empty")
            (let ((toRet (front-queue)))
                 (set! q (cdr q))
                 (set-car! q '())
                 toRet)))
    (define (dispatch op)
      (cond ((eq? op 'empty-queue?) empty-queue?)
            ((eq? op 'front-queue) front-queue)
            ((eq? op 'insert!) insert!)
            ((eq? op 'remove!) remove!)
            ((eq? op 'list) (cdr q))
            (else (error "Incorrect option -- MAKE-QUEE"))))
    dispatch)

(define (empty-queue? q)
    ((q 'empty-queue?)))

(define (front-queue q)
    ((q 'front-queue)))

(define (insert-queue! q i)
    ((q 'insert!) i))

(define (delete-queue! q)
    ((q 'remove!)))

;; testing
;; (define q (make-queue))
;; (insert-queue! q 'a)    a
;; (insert-queue! q 'b)    a b
;; (delete-queue! q)       b
;; (insert-queue! q 'c)    b c
;; (insert-queue! q 'd)    b c d
;; (delete-queue! q)       c d


; excercise 3.21
(define (print-queue q) (display (front-ptr q)))


; excercise 3.30


(define (ripple-carry-adder as bs ss c)
    (let ((cs (cons c (map (lambda _ (wire)) (cdr as)))))
      (for-each full-adder as bs ss cs)))


; excercise 3.33

(define (averager a b c)
    (let ((v (make-connector))
          (w (make-connector)))
       (multiplier v w c)
       (adder a b v)
       (constant 1/2 w)
       'ok))


; stream from scratch

(define (range i n)
  (define (loop i)
    (lambda ()
     (if (> i n)
         '()
         (cons i (loop (+ i 1))))))
  
  (define p (cons '() (loop i)))
  (define listRange p)
  
  (define (next)
       (if (not (null? p))
           (begin (set-cdr! p ((cdr p)))
                  (set! p (cdr p)))))
  
  (define (dispatch op)
          (cond ((eq? op 'next) (next) (cdr listRange))
                ((eq? op 'list) (cdr listRange))))
  dispatch)

; testing
;; (define a (range 1 100))
;; (a 'next)
;; (for-each (lambda _ (a 'next)) (iota 10))
;; (a 'list)

;; (define b (map (lambda (n) (+ n 2)) (a 'list)))


; excercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

