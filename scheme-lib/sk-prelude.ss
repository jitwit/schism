(library (sk-prelude)
  (export nullo ;; list relations
          conso caro cdro pairo listo
          eqo eq-caro
          appendo rembero
          anyo ;; nevero
          ;; numerical
          int->bits zeroo poso >1o
          full-addero
          element-of common-element
          natural natural* negative integer)
  (import (rnrs) (sk))

  ;;;; Various Relations
  (define (nullo x)
    (== '() x))

  (define (eqo x y)
    (== x y))

  (define (eq-caro l x)
    (caro l x))

  (define (conso a d x)
    (== (cons a d) x))

  (define (caro x a)
    (call/fresh
     (lambda (d)
       (conso a d x))))

  (define (cdro x d)
    (call/fresh
     (lambda (a)
       (conso a d x))))

  (define (pairo x)
    (call/fresh
     (lambda (a)
       (call/fresh
        (lambda (d)
          (conso a d x))))))

  (define (listo l)
    (disj (nullo l)
          (call/fresh
           (lambda (l*)
             (conj (cdro l l*)
                   (listo l*))))))
  (define (element-of var lst)
    (if (null? lst)
        fail
        (disj (== var (car lst))
              (element-of var (cdr lst)))))

  (define (common-element var l1 l2)
    (conj (element-of var l1) (element-of var l2)))

  (define (membero x l)
    (disj*
     `(,(conj (nullo l) fail)
       ,(caro l x)
       ,(call/fresh
         (lambda (tl)
           (conj (cdro l tl) (membero x tl)))))))

  (define (rembero x l out)
    (disj*
     `(,(conj (nullo l) (== '() out))
       ,(conj (eq-caro l x) (cdro l out))
       ,(call/fresh
         (lambda (a)
           (call/fresh
            (lambda (d)
              (call/fresh
               (lambda (res)
                 (conj*
                  `(,(conso a d l)
                    ,(rembero x d res)
                    ,(conso a res out))))))))))))

  (define (appendo x y z)
    (disj (conj (== x '()) (== y z))
          (call/fresh
           (lambda (hd)
             (call/fresh
              (lambda (tl)
                (call/fresh
                 (lambda (z*)
                   (conj*
                    `(,(conso hd tl x)
                      ,(conso hd z* z)
                      ,(appendo tl y z*)))))))))))

  (define (anyo g)
    (ife g succeed (anyo g)))

  ;; (define nevero (anyo fail))
  ;; (define alwayso (anyo succeed))
  (define (int->bits n)
    (cond
     ((zero? n) '())
     ((and (not (zero? n)) (even? n))
      (cons 0 (int->bits (bitwise-arithmetic-shift-right n 1))))
     (else
      (cons 1 (int->bits (bitwise-arithmetic-shift-right n 1))))))

  (define (zeroo n)
    (== '() n))

  (define (poso n)
    (call/fresh
     (lambda (a)
       (call/fresh
        (lambda (d)
          (== `(,a . ,d) n))))))
  
  (define (>1o n)
    (call/fresh
     (lambda (a)
       (call/fresh
        (lambda (ad)
          (call/fresh
           (lambda (dd)
             (== `(,a ,ad . ,dd) n))))))))

  (define (full-addero b x y r c)
    (disj*
     `(,(conj* `(,(== 0 b) ,(== 0 x) ,(== 0 y) ,(== 0 r) ,(== 0 c)))
       ,(conj* `(,(== 1 b) ,(== 0 x) ,(== 0 y) ,(== 1 r) ,(== 0 c)))
       ,(conj* `(,(== 0 b) ,(== 1 x) ,(== 0 y) ,(== 1 r) ,(== 0 c)))
       ,(conj* `(,(== 0 b) ,(== 0 x) ,(== 1 y) ,(== 1 r) ,(== 0 c)))
       ,(conj* `(,(== 1 b) ,(== 1 x) ,(== 0 y) ,(== 0 r) ,(== 1 c)))
       ,(conj* `(,(== 1 b) ,(== 0 x) ,(== 1 y) ,(== 0 r) ,(== 1 c)))
       ,(conj* `(,(== 0 b) ,(== 1 x) ,(== 1 y) ,(== 0 r) ,(== 1 c)))
       ,(conj* `(,(== 1 b) ,(== 1 x) ,(== 1 y) ,(== 1 r) ,(== 1 c))))))

  (define (natural* x n)
    (disj (== x n)
          (lambda (s)
            (lambda ()
              ((natural* x (+ n 1)) s)))))
  
  (define (natural x)
    (natural* x 0))

  (define (negative* x n)
    (disj (== x n)
          (lambda (s)
            (lambda ()
              ((negative* x (- n 1)) s)))))

  (define (negative x)
    (negative* x -1))

  (define (integer x)
    (disj (natural x)
          (negative x)))


  )
