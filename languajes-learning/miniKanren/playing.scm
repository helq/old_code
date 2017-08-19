(load "lib/miniKanren-wrappers")

; examples here are taken mainly from the book "The reasoned schemer"

; copied from microKanren implementation
(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

(define (nullo l)
  (== l '() ))

#|
 |(run* (x)
 |  (nullo o))
 |#

(define (pairo p)
  (fresh (a d)
    (== (cons a d) p)))

#|
 |(run* (x)
 |  (pairo x))
 |#

(define (conso a p l)
  (== (cons a p) l))

#|
 |(run* (x)
 |  (conso x '(b) '((a c) b)))
 |#

(define (listo l)
  (conde
    ((nullo l))
    ((pairo l)
     (fresh (a p)
       (conso a p l)
       (listo p)))))

(define (listo l)
  (conde
    ((== l '()))
    ((fresh (a p)
       (== `(,a . ,p) l)
       (listo p)))))

#|
 |(run 5 (l)
 |  (listo l))
 |#

(define mapo
  (lambda (fo l out)
    (conde
      ((nullo l) (== out '() ))
      ((pairo l)
       (fresh (a d aout dout)
          (conso a d l)
          (conso aout dout out)
          (fo a aout)
          (mapo fo d dout))))))

(define (singletono x y)
  (== `(,x) y))

#|
 |(run* (z) (singletono 'a z))
 |
 |(run* (x)
 |  (mapo singletono '() x))
 |
 |(run* (x)
 |  (mapo singletono '(1 2 3 4) x))
 |#

(define (twinof x y)
  (== `(,y ,y) x))

#|
 |(run* (x)
 |  (mapo twinof '((1 1) (2 2) (5 5)) x))
 |
 |(run* (x)
 |  (mapo (flip twinof) '(a b r) x))
 |#

#|
 |; faulty, don't use
 |(define (addoneo x y) 
 |  (== (+ x 1) y)) ; wrong! miniKanren doesn't understand normal operations with numbers
 |
 |(run* (x)
 |  (mapo addoneo '(1 2 3 4) x))
 |#

(define (lolo1 l)
  (conde
    ((nullo l))
    ((fresh (a d)
        (conso a d l)
        (listo a)
        (lolo1 d)))))

#|
 |(run 10 (x) (lolo1 x))
 |#

(define (lolo2 l)
  (conde
    ((nullo l))
    ((fresh (a d)
        (conso a d l)
        (lolo1 d)
        (listo a)))))

#|
 |(run 10 (x) (lolo1 x))
 |; why does this diverge from the book?, the book offers a different answer for what the last line
 |; returns using this implementation. (ans: probably, it has to do with the search algorithm used by
 |; this implementation, different from the one in the book)
 |; answer: in the book condi is in fact what now everyone refers as conde, nobody uses the old conde
 |#

(define (listofo p l)
  (conde
    ((nullo l))
    ((fresh (a d)
      (conso a d l)
      (p a)
      (listofo p d)))))

#|
 |(run 50 (x)
 |  (listofo (lambda (y) (== y 'a)) x))
 |#

(define (lolo3 l)
  (listofo listo l))

#|
 |(run 10 (x) (lolo3 x))
 |#

(define (membero e l)
  (fresh (a d)
     (conso a d l)
     (conde
       ((== a e))
       ((membero e d)))))

#|
 |(run* (x)
 |   (membero 'a '(b a c))
 |   (== x #t))
 |
 |(run* (x)
 |   (membero x '(b a c)))
 |
 |(run 5 (x)
 |   (membero 'a x))
 |#

; forcing `l` to be a list. `e` is a member of a proper list
(define (pmembero e l)
  (fresh (a d)
     (conso a d l)
     (listo d)
     (conde
       ((== a e))
       ((pmembero e d)))))

#|
 |(run 5 (x)
 |   (listo x))
 |
 |(run 20 (x)
 |   (pmembero 'a x))
 |
 |(run* (x)
 |   (pmembero x '(1 2 3 4)))
 |#

; definition of memberrevo, "reversed" version of membero, it doesn't work as expected
; with this implementation, conde works different here as it does in the book
(define (memberrevo e l)
  (fresh (a d)
     (conso a d l)
     (conde
       ((memberrevo e d))
       ((== a e)))))

#|
 |(run* (x)
 |   (memberrevo x '(b a c)))
 |#

; from book memberrevo. It doesn't work as expected!
(define (caro l a)
  (fresh (d)
    (== `(,a . ,d) l)))

(define (eq-caro l e)
     (caro l e))

#|
 |(run* (x) (eq-caro '(1 2 3) x))
 |#

(define (cdro l d)
  (fresh (a)
    (== `(,a . ,d) l)))

#|
 |; another definitions, shouldn't be used anyway
 |(define membero
 |  (lambda (x l)
 |    (conde
 |      ((eq-caro l x))
 |      ((fresh (d)
 |          (cdro l d)
 |          (membero x d))))))
 |
 |(run* (x)
 |   (membero x '(b a c)))
 |
 |(define memberrevo
 |  (lambda (x l)
 |    (conde
 |      ((fresh (d)
 |          (cdro l d)
 |          (memberrevo x d)))
 |      ((eq-caro l x)))))
 |
 |(run* (x)
 |   (memberrevo x '(b a c)))
 |#

(define (rembero e l out)
  (conde
    ((== '() l) (== '() out))
    ((fresh (d)
        (== `(,e . ,d) l)
        (== d out)
        ))
    ((fresh (a d aout dout)
        (== `(,a . ,d) l)
        (== `(,aout . ,dout) out)
        (== a aout)
        (rembero e d dout)))))

#|
 |(run 1 (x) (rembero 'a '() x))
 |
 |(run* (x) (rembero 'a '() x))
 |
 |(run* (x) (rembero 'a '(a b c d) x))
 |
 |(run* (x) (rembero 'c '(a b c d) x))
 |
 |(run* (x)
 |  (fresh (y)
 |     (rembero y '(a b c d) x)))
 |
 |(run* (x)
 |  (rembero 'a x '(a b c d)))
 |
 |(run 10 (r)
 |  (fresh (x y out)
 |    (rembero x `(a ,y c d) out)
 |    (== (cons x y) r)))
 |
 |(run 10 (r)
 |  (fresh (x y)
 |    (rembero x `(a ,y c d) '(a b c d))
 |    (== (cons x y) r)))
 |
 |(run* (x) ; this shouldn't make any sense, but it douse because see next
 |  (rembero 'b '(a b c d) '(a b c d))
 |  (== x #t))
 |
 |(run* (out)
 |  (rembero 'b '(a b c d) out))
 |
 |(run 13 (w)
 |  (fresh (y z out)
 |     (rembero y `(a b ,y d ,z . ,w) out)))
 |
 |(run 1 (out)
 |   (fresh (y)
 |      (rembero 'peas `(a b ,y d peas e) out)))
 |
 |(run* (out)
 |  (fresh (y z)
 |    (rembero y `(a b ,y d ,z e) out)))
 |#

(define (surpriseo x)
  (rembero x '(a b c) '(a b c)))

#|
 |(run* (r)
 |  (== 'd r)
 |  (surpriseo r))
 |
 |(run* (r)
 |  (surpriseo r)) ; this shouldn't return _.0, it shouldn't be a "free" variable, it is restricted not to be of a certaint type
 |
 |(run* (r)
 |  (== 'a r)
 |  (surpriseo r)) ; dafuq? (look at the note above)
 |#


; chapter 6

#|
 |(run 2 (x)
 |  (== x 'a))
 |
 |(run 7 (r) ; <- again, not even remotely like in the book (5.29) :S
 |  (fresh (x y)
 |      (appendo x y `(cake with ice d t))
 |      (== `(,x ,y) r)))
 |#

(define (anyo g)
  (conde
    (g)
    ((anyo g))))

;(define succed (== 'a 'a))
;(define fail   (== 'a 'b))
(define (succed x) (unit x))  ; better definition
(define (fail x)    mzero)

#|
 |(run* (x) succed (== x 'b))
 |(run* (x) fail   (== x 'b))
 |#

(define (tripleit x) `(,x ,x ,x))
#|
 |(run* (x) tripleit (== x 'b))
 |#

(define nevero
  (anyo fail))
(define alwayso
  (anyo succed))

#|
 |(run 5 (x)
 |  alwayso
 |  (== x 'b))
 |
 |(run 1 (q) ; stays in a loop forever
 |  nevero
 |  (== #t q))
 |
 |(run 1 (q)
 |  fail
 |  nevero
 |  (== #t q))
 |#

(define (salo g)
  (conde
    (succed)
    (g)))

#|
 |(run 1 (q)
 |  (salo nevero)
 |  (== #t q))
 |
 |(run 2 (q) ; stays in the loop forever
 |  (salo nevero)
 |  (== #t q))
 |
 |(run 1 (q) ; nop, it behaves different to the book version (6.18) :S
 |  (conde   ; (because here conde behaves as condi! read https://stackoverflow.com/a/10850364 )
 |    ((== #f q) alwayso)
 |    ((anyo (== #t q))))
 |  (== #t q))
 |
 |(run 2 (q)
 |  (conde
 |    ((== #f q) alwayso)
 |    ((== #t q)))
 |  (== #t q))
 |#

(define (teacupo x)
  (conde
    ((== x 'tea))
    ((== x 'cup))))

#|
 |(run* (y)
 |  (teacupo y))
 |
 |(run 5 (y) ; (6.24)
 |  (conde
 |    ((teacupo y))
 |    ((== #t y))))
 |; out: (#t tea cup) ; <- doesn't work like in the book
 |
 |(run 5 (y) ; <- modifying last run, something close to what in the book happens
 |  (conde   ;    it's probably the way in which the search is performed in the implementations
 |    ((teacupo y))
 |    ((conde ((conde ((== #t y))))))))
 |; out: (tea cup #t)
 |
 |(run 5 (r)
 |  (conde
 |    ((== #t r) alwayso)
 |    ((== #f r) alwayso)))
 |
 |(run 5 (q)
 |  (conde
 |    (alwayso)
 |    (nevero))
 |  (== q #t))
 |#

; === numbers ===

(define (build-num n)
  (cond
    ((zero? n) '())
    (else
     (let ((b (modulo n 2))
           (m (quotient n 2)))
       (cons b (build-num m))))))

(define (poso n)
  (fresh (x y)
    (== n `(,x . ,y))))

#|
 |(run* (r)
 |  (poso r))
 |
 |(run* (r)
 |  (poso (build-num 5))
 |  (== r #t))
 |
 |(run* (r)
 |  (poso (build-num 0))
 |  (== r #t))
 |
 |(run* (r)
 |  (poso r)
 |  (== r (build-num 1)))
 |
 |(run* (r)
 |  (disj+
 |    (== r (build-num 0))
 |    (== r (build-num 1))
 |    (== r (build-num 2)))
 |  (poso r))
 |#

(define (>1o n)
  (fresh (x y w)
    (== n `(,x ,y . ,w))))

#|
 |(run* (r)
 |  (disj+
 |    (== r (build-num 0))
 |    (== r (build-num 1))
 |    (== r (build-num 2)))
 |  (>1o r))
 |#

(define (full-addero b x y r c)
  (conde
    ((== b 0) (== x 0) (== y 0) (== r 0) (== c 0))
    ((== b 1) (== x 0) (== y 0) (== r 1) (== c 0))
    ((== b 0) (== x 1) (== y 0) (== r 1) (== c 0))
    ((== b 1) (== x 1) (== y 0) (== r 0) (== c 1))
    ((== b 0) (== x 0) (== y 1) (== r 1) (== c 0))
    ((== b 1) (== x 0) (== y 1) (== r 0) (== c 1))
    ((== b 0) (== x 1) (== y 1) (== r 0) (== c 1))
    ((== b 1) (== x 1) (== y 1) (== r 1) (== c 1))
))

#|
 |(run* (p)
 |  (fresh (b x y r c)
 |    (full-addero b x y r c)
 |    (== `(,b ,x ,y ,r ,c) p)))
 |#

(define (succo m n)
  (conde
    ((nullo m) (== n '(1)))
    ((poso m) (>1o n)
     (fresh (am dm an dn)
        (== `(,am . ,dm) m)
        (== `(,an . ,dn) n)
        (conde
          ((== am 0) (== an 1) (== dm dn))
          ((== am 1) (== an 0) (succo dm dn)))))))

#|
 |(run* (r)
 |  (succo (build-num 2) (build-num 3))
 |  (== r #t))
 |
 |(run* (r)
 |  (succo (build-num 2) (build-num 4))
 |  (== r #t))
 |
 |(run* (r)
 |  (succo r (build-num 2)))
 |
 |(run* (r)
 |  (succo r (build-num 3)))
 |
 |(run* (r)
 |  (succo r (build-num 0))) ; beautiful, there is no predecessor for zero in the natural numbers :')
 |
 |(run 10 (n)
 |  (fresh (m) (succo m n)))
 |
 |(run 10 (n)
 |  (fresh (m) (succo n m)))
 |#

#|
 |; my definition
 |(define (addero b m n o)
 |  (conde ; todo: add cases that make this definition not repeat values
 |    ((== b 0) (conde
 |                ((nullo n) (== m o))
 |                ((poso n)   (nullo m) (== n o))))
 |    ((== b 1) (conde
 |                ((nullo n) (succo m o))
 |                ((poso n)  (nullo m) (succo n o))))
 |    ((poso m) (poso n)
 |     (fresh (am dm an dn c ao do_)
 |        (== `(,am . ,dm) m)
 |        (== `(,an . ,dn) n)
 |        (== `(,ao . ,do_) o)
 |        (full-addero b am an ao c)
 |        (addero c dm dn do_)))))
 |#
; book's definition
(define (addero d n m r)
  (conde 
    ((== 0 d) (== '() m) (== n r))
    ((== 0 d) (== '() n) (== m r) (poso m))
    ((== 1 d) (== '() m) (addero 0 n '(1) r))
    ((== 1 d) (== '() n) (poso m) (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
      (fresh (a c)
         (== `(,a ,c) r)
         (full-addero d 1 1 a c)))
    ((== '(1) n) (gen-addero d n m r))
    ((== '(1) m) (>1o n) (>1o r) (addero d '(1) n r))
    ((>1o n) (gen-addero d n m r))
    ))
(define (gen-addero d n m r)
  (fresh (a b c e x y z)
     (== `(,a . ,x) n)
     (== `(,b . ,y) m) (poso y)
     (== `(,c . ,z) r) (poso z)
     (full-addero d a b c e)
     (addero e x y z)))

#|
 |(run* (r)
 |  (addero 0 '() '() r)) ; 0 + 0 = 0
 |
 |(run* (r)
 |  (addero 0 (build-num 3) '() r)) ; 3 + 0 = 3
 |
 |(run* (r)
 |  (addero 1 (build-num 3) '() r)) ; 1 + 3 + 0 = 4
 |
 |(run* (r)
 |  (addero 0 (build-num 3) (build-num 1) r)) ; 3 + 1 = 4
 |
 |(run* (r)
 |  (addero 0 (build-num 3) (build-num 8) r)) ; 3 + 8 = 11
 |#

(define (get-num n)
  (cond
    ((null? n) 0)
    (else (+ (car n) (* 2 (get-num (cdr n)))))))

#|
 |(get-num (build-num 10))
 |
 |(map get-num
 | (run* (r)
 |  (addero 0 (build-num 3) (build-num 8) r))) ; 3 + 8 = 11
 |
 |(map (lambda (l) (map get-num l))
 | (run* (r)
 |  (fresh (x y)
 |    (addero 0 x y (build-num 8))
 |    (== `(,x ,y) r))))
 |
 |(run 13 (p)
 |  (fresh (x y r)
 |    (addero 0 x y r)
 |    (== p `(,x ,y ,r))))
 |#

(define (numberposo n)
  (conde
    ((== n '(1)))
    ((fresh (a d)
       (== `(,a . ,d) n)
       (disj (== a 0) (== a 1))
       (numberposo d)))))

(define (numbero n)
  (conde
    ((== n '()))
    ((numberposo n))))

#|
 |(run 20 (n) (numbero n))
 |
 |(map get-num (run 20 (n) (numbero n)))
 |
 |(run 13 (p)
 |  (fresh (x y r)
 |    (addero 0 x y r)
 |    (numbero x)
 |    (numbero y)
 |    (numbero r)
 |    (== p `(,x ,y ,r))))
 |
 |(map (lambda (l) (cons (get-num (car l)) (get-num (cdr l))))
 | (run* (r)
 |  (fresh (x y)
 |    (addero 0 x y (build-num 8))
 |    (numbero x)
 |    (numbero y)
 |    (== `(,x . ,y) r))))
 |#

(define (eveno n)
  (fresh (a d)
     (== `(,a . ,d) n)
     (== a 0)))

(define (oddo n)
  (fresh (a d)
     (== `(,a . ,d) n)
     (== a 1)))

#|
 |(map (lambda (l) (cons (get-num (car l)) (get-num (cdr l))))
 | (run* (r)
 |  (fresh (x y)
 |    (oddo x)
 |    (addero 0 x y (build-num 13))
 |    (numbero x)
 |    (numbero y)
 |    (== `(,x . ,y) r))))
 |#

(define (+o m n k)
  (addero 0 m n k))

#|
 |(map (lambda (l) (map get-num l))
 |  (run* (r)
 |    (fresh (x y)
 |       (disj+
 |         (== x '(1 0 1))
 |         (== x '(0 1 1))
 |         (== x '(0 1 0 1)))
 |       (+o x y (build-num 10))
 |       (== `(,x ,y) r))))
 |#

; === chapter 8 ===
; my definition
(define (*o n m k)
  (conde
    ((== n '())  (== k '()))
    ((== n '(1)) (== k m))
    ((>1o n)
     (fresh (x y)
        (succo x n)
        (*o x m y)
        (+o y m k)
        ))))

#|
 |(map get-num ; damn, takes a while :S
 |  (run 6 (r)
 |    (fresh (x)
 |      (*o x r (build-num 12)))))
 |#

; second attemp, trying to figure out how is the code from the book without looking at it
(define (*o n m k)
  (conde
    ((== n '())  (== k '()))
    ((== n '(1)) (== k m))
    ((poso n) (== m '()) (== k '()))
    ((>1o n) (== m '(1)) (== k n))
    ((>1o n) (>1o m)
     (fresh (x y)
        (succo x n)
        (*o x m y)
        (+o y m k)
        ))))

#|
 |(run* (n)
 |  (*o (build-num 2) (build-num 2) n))
 |
 |(run 34 (r)
 |  (fresh (x y z)
 |    (*o x y z)
 |    (== `(,x ,y ,z) r)))
 |
 |(map get-num ; now it's faster, yey
 |  (run 6 (r)
 |    (fresh (x)
 |      (*o x r (build-num 12)))))
 |
 |(map get-num ; but this is still slower
 |  (run 8 (r)
 |    (fresh (x)
 |      (*o x r (build-num 24)))))
 |#
