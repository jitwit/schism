;; Copyright 2020 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the License);
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an AS IS BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(library (pre-post-order)
  (export do-test)
  (import (rnrs)
          (schism))
  (define (trace x) (write x) (newline) x)
  ;; type TREE = SXML-NODE | SXML-NODELIST
  ;; type BINDINGS = (BINDING ...)
  ;; type BINDING = (TRIGGER-SYMB *preorder* . HANDLER)
  ;;              | (TRIGGER-SYMB *macro* . HANDLER)
  ;;              | (TRIGGER-SYMB NEW-BINDINGS . HANDLER)
  ;;              | (TRIGGER-SYMB . HANDLER)
  ;; type TRIGGER-SYMB = SXML-NAME | *text* | *default*
  ;; type NEW-BINDINGS = BINDINGS
  ;; type HANDLER = TRIGGER-SYMB x TREE -> TREE
  (define default cons) ;; (cons tag tree), ie (lambda x x)
  (define (const x y) y)
  (define node-tag car)
  ;;;; macro definitions and quasiquotation
  (define (quasicons head tail)
    (if (and (pair? tail) (eq? (car tail) 'quote)
             (pair? (cdr tail)) (null? (cddr tail))
             (pair? head) (eq? (car head) 'quote)
             (pair? (cdr head)) (null? (cddr head)))
        `'(,(cadr head) . ,(cadr tail))
        `(cons ,head ,tail)))  
  (define (expand-quasiquote expr level)
    (if (pair? expr)
        (let ((head (car expr))
              (tail (cdr expr)))
          (cond
           ((and (eq? head 'unquote) (pair? tail) (null? (cdr tail)))
            (if (zero? level)
                (car tail);; (expand-macros (car tail))
                (quasicons ''unquote (expand-quasiquote tail (- level 1)))))
           ((and (eq? head 'quasiquote) (pair? tail) (null? (cdr tail)))
            (quasicons ''quasiquote
                       (expand-quasiquote tail (+ level 1))))
           (else
            (quasicons (expand-quasiquote head level)
                       (expand-quasiquote tail level)))))
        `',expr))
  (define (expand-or or tree)
    (cond
     ((null? tree) '#f)
     ((null? (cdr tree)) (car tree))
     (else
      (let ((t (gensym "t")))
        `(let ((,t ,(car tree)))
           (if ,t ,t ,(expand-or or (cdr tree))))))))
  (define (expand-and and tree)
    (cond
     ((null? tree) '#t)
     ((null? (cdr tree)) (car tree))
     (else `(if ,(car tree) ,(expand-and and (cdr tree)) #f))))
  (define (expand-when when tree)
    `(if ,(car tree) (begin . ,(cdr tree)) (begin)))
  (define (expand-unless unless tree)
    `(if ,(car tree) (begin) (begin . ,(cdr tree))))
  (define (expand-not not tree)
    `(if ,(car tree) #f #t))
  (define (expand-let* let* tree)
    (let ((bindings (car tree)))
      (if (null? bindings)
          `(begin . ,(cdr tree))
          (let ((x (caar bindings))
                (e (cadar bindings))
                (rest (cdr bindings)))
            `(let ((,x ,e))
               ,(expand-let* let* (cons rest (cdr tree))))))))
  (define (expand-cond cond tree)
    (let ((clause (car tree))
          (rest (cdr tree)))
      (cond ((eq? (car clause) 'else)
             (unless (null? rest)
               (error 'expand-cond "misplaced else keyword"))
             `(begin . ,(cdr clause)))
            (else
             `(if ,(car clause)
                  (begin . ,(cdr clause))
                  (cond . ,rest))))))

  ;;;; default macro bindings
  (define *schism-macros*
    `((quasiquote *macro* . ,(lambda (qq tree) (expand-quasiquote tree 0)))
      (cond *macro*       . ,expand-cond)
      (or *macro*         . ,expand-or)
      (and *macro*        . ,expand-and)
      (let* *macro*       . ,expand-let*)
      (when *macro*       . ,expand-when)
      (unless *macro*     . ,expand-unless)
      (not *macro*        . ,expand-not)
      (*leaf*             . ,(lambda (tag leaf) leaf))
      (*default*          . ,default)))

  (define (pre-post-order tree bindings)
    (let ((default (assq '*default* bindings))
          (leaf    (assq '*leaf* bindings)))
      (walk tree bindings leaf default)))
  ;; leaft & defaultt are pairs to simplify assq binding lookup handling
  (define (walk tree bindings leaf default)
    (cond
     ((null? tree) tree)
     ((not (pair? tree)) ((cdr leaf) 'leaf tree))
     (else
      (let* ((trigger (node-tag tree))
             (binding (or (assq trigger bindings) default)))
        (cond
         ((not binding) (error tree 'pre-post-order "unbound node"))
         ((not (pair? (cdr binding))) ; procedure
          ((cdr binding) trigger (map (lambda (child)
                                        (walk child bindings leaf default))
                                      (cdr tree))))
         ((eq? '*macro* (cadr binding))
          (walk ((cddr binding) trigger (cdr tree)) bindings leaf default))
         ((eq? '*preorder* (cadr binding))
          ((cddr binding) trigger (cdr tree)))
         (else ;; extra bindings introduced in cadr
          (let* ((bindings (append (cadr binding) bindings))
                 (default  (assq '*default* bindings))
                 (leaf     (assq '*leaf* bindings)))
            ((cddr binding) trigger
             (walk (cdr tree) bindings leaf default)))))))))
  (define when-eg
    '(let ((x 2)) (when (= x 2) (display 'woohoo))))
  (define not-eg
    '(if (not #t) 'a 'b))
  (define and-eg
    '(and x y (or (cdr w) z)))
  (define or-eg
    '(or x `y z))
  (define let*-eg
    '(let* ((x 12) (y (+ x x)))
       (* y x)))
  (define quasi-eg
    '(let ((x '(1 2 3)))
       `(,x 3 ,x)))
  (define cond-eg
    '(cond ((= x 1) "one")
           ((= x 2) "two")
           (else 'something)))
  (define expr-eg
    '((lambda (x) x) (lambda (x) x)))
  (define examples
    `((when . ,when-eg)
      (not . ,not-eg)
      (and . ,and-eg)
      (or . ,or-eg)
      (let* . ,let*-eg)
      (quasiquote . ,quasi-eg)
      (cond . ,cond-eg)
      (other . ,expr-eg)))
  (define (expand-macros expr)
    (pre-post-order expr *schism-macros*))
  (define (do-test)
    (map (lambda (eg)
           (newline)
           (trace (cdr eg))
           (trace (expand-macros (cdr eg))))
         examples)
    #t))
