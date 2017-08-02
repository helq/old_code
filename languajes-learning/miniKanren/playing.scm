(load "lib/miniKanren-wrappers")

;copied from microKanren implementation
(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

(define (nullo l)
  (== l '() ))

(run* (x)
  (nullo o))

(define (pairo p)
  (fresh (a d)
    (== (cons a d) p)))

(run* (x)
  (pairo x))

(define (conso a p l)
  (== (cons a p) l))

(run* (x)
  (conso x '(b) '((a c) b)))

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

(run 5 (l)
  (listo l))

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

(run* (z) (singletono 'a z))

(run* (x)
  (mapo singletono '() x))

(run* (x)
  (mapo singletono '(1 2 3 4) x))

(define (twinof x y)
  (== `(,y ,y) x))

(run* (x)
  (mapo twinof '((1 1) (2 2) (5 5)) x))

#|
 |(define (addoneo x y) 
 |  (== (+ x 1) y)) ; wrong! miniKanren doesn't understand normal operations with numbers
 |
 |(run* (x)
 |  (mapo addoneo '(1 2 3 4) x))
 |#
