
(library (sk)
  ;; doesn't notice if something not defined isn't exported 
  (export var var?
          walk walk*
          extend-s unify
          succeed fail
          ==
          map-$
          call/fresh
          run*
          run
          disj disj-i disj*
          conj conj-i conj*
          ife
          )
  (import (rnrs))

  ;; The uKanren paper uses vectors to wrap variables. For now, we
  ;; tag pairs.
  (define (var name)
    `(var . ,name))

  (define (var? x)
    (and (pair? x) (eq? 'var (car x))))

  (define (var=? x y)
    (eq? (cdr x) (cdr y)))

  ;;;; Unification
  (define (walk var s)
    (let ((binding (and (var? var) (assp (lambda (u) (var=? u var)) s))))
      (if binding (walk (cdr binding) s) var)))

  (define (extend-s var val s)
    `((,var . ,val) . ,s))

  (define (occurs-check x v s)
    (let ((v (walk v s)))
      (cond
       ((var? v) (eq? v x))
       ((pair? v) 
        (or 
         (occurs-check x (car v) s)
         (occurs-check x (cdr v) s)))
       (else #f))))

  (define (ext-s-check x v s)
    (and (not (occurs-check x v s))
         (extend-s x v s)))

  (define (unify u v s)
    (let ((u (walk u s)) (v (walk v s)))
      (cond
       ((var? u) (extend-s u v s))
       ((var? v) (extend-s v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s))))
       ;; equal?
       (else (and (eq? u v) s)))))

  (define (unify-check u v s)
    (let ((u (walk u s)) (v (walk v s)))
      (cond
       ((eq? u v) s)
       ((var? u) (ext-s-check u v s))
       ((var? v) (ext-s-check v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify-check (car u) (car v) s)))
          (and s (unify-check (cdr u) (cdr v) s))))
       ;; equal?
       (else (and (eq? u v) s)))))

  ;;; A more diligent wallk.
  (define (walk* var s)
    (let ((v (walk var s)))
      (cond
       ((var? v) v)
       ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
       (else v))))
  
  ;;;; Core
  ;;; uKanren paper refers to these as unit and mzero.
  (define (succeed s.c) `(,s.c))
  (define (fail s.c) '())
  (define (== u v)
    (lambda (s.c)
      (lambda ()
        (let ((s (unify u v (car s.c))))
          (if s (succeed `(,s . ,(cdr s.c))) (fail s))))))

  (define (==-check u v)
    (lambda (s.c)
      (lambda ()
        (let ((s (unify-check u v (car s.c))))
          (if s (succeed `(,s . ,(cdr s.c))) (fail s))))))

  ;;;; Streams
  ;;; To map over a possibly infinite stream.
  (define (map-$ f $)
    (cond
     ((null? $) '())
     ((procedure? $) (map-$ f ($)))
     (else (cons (f (car $)) (lambda () (map-$ f (cdr $)))))))

  (define (pull $)
    (if (procedure? $) ($) $))

  (define (take-all $)
    (let (($ (pull $)))
      (if (null? $)
          $
          (cons (car $) (take-all (cdr $))))))

  (define (take n $)
    (if (zero? n)
        '()
        (let (($ (pull $)))
          (if (null? $)
              '()
              (cons (car $) (take (- n 1) (cdr $)))))))

  ;;; Use int c in s.c pair to introduce a new variable to give to f
  ;;; to build a goal.
  (define (call/fresh g)
    (lambda (s.c)
      (lambda ()
        (let ((c (cdr s.c)))
          ((g (var c)) `(,(car s.c) . ,(+ c 1)))))))

  (define (run n var goal)
    (let ((result (goal '(() . 0))))
      (take n (map-$ (lambda (s.c) (walk* var (car s.c))) result))))

  (define (run* var goal)
    (let ((result (goal '(() . 0))))
      (take-all (map-$ (lambda (s.c) (walk* var (car s.c))) result))))

  ;;;; Conjunction/Disjunction
  ;;; conj/disj use interleave or choice if suffixed with -i to
  ;;; advance their results.
  (define (disj g1 g2)
    (lambda (s.c)
      (interleave (g1 s.c) (g2 s.c))))

  (define (disj-i g1 g2)
    (lambda (s.c)
      (choice (g1 s.c) (g2 s.c))))

  (define (conj g1 g2)
    (lambda (s.c)
      (bind (g1 s.c) g2)))

  (define (conj-i g1 g2)
    (lambda (s.c)
      (bind-i (g1 s.c) g2)))

  ;;; Conjunct a list of goals. Kanrens normally use syntax to
  ;;; accomplish this.
  (define (conj* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((conj (car gs) (conj* (cdr gs))) s.c)))
       (else (succeed s.c)))))

  (define (conj-i* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((conj-i (car gs) (conj-i* (cdr gs))) s.c)))
       (else (succeed s.c)))))

  (define (disj* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((disj (car gs) (disj* (cdr gs))) s.c)))
       (else (fail s.c)))))

  (define (disj-i* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((disj-i (car gs) (disj-i* (cdr gs))) s.c)))
       (else (fail s.c)))))

  ;;; The uKanren paper uses the haskellism mplus. Two streams take
  ;;; turns advancing.
  (define (interleave $1 $2)
    (cond
     ((null? $1) $2)
     ((procedure? $1) (lambda () (interleave $2 ($1))))
     (else (cons (car $1) (lambda () (interleave $2 (cdr $1)))))))  

  ;;; This one is otherwise called mplusi. The distinction from
  ;;; interleave is that one stream is exhausted before the next is
  ;;; tried.
  (define (choice $1 $2)
    (cond
     ((null? $1) $2)
     ((procedure? $1) (lambda () (choice ($1) $2)))
     (else (cons (car $1) (lambda () (choice (cdr $1) $2))))))

  ;;; g is a goal whose results must agree with those coming from $.
  (define (bind $ g)
    (cond ((null? $) '())
          ((procedure? $) (lambda () (bind ($) g)))
          (else (interleave (g (car $)) (bind (cdr $) g)))))

  (define (bind-i $ g)
    (cond ((null? $) '())
          ((procedure? $) (lambda () (bind-i ($) g)))
          (else (choice (g (car $)) (bind-i (cdr $) g)))))

  (define (ife g1 g2 g3)
    (lambda (s.c)
      (lambda () (interleave ((conj g1 g2) s.c)
                        (lambda () (g3 s.c))))))

  )
