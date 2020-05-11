;; Copyright 2019 Google LLC
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

(library
    (trivial)
  (export do-test)
  (import (rnrs)
          (sk)
          (sk-prelude))

  (define (display-ln x)
    (display x) (newline))
  
  (define (do-test)
    (let ((empty-state '(() . 0))
          (vx (var 'x))
          (vy (var 'y))
          (vz (var 'z))
          (vq (var 'q)))
;;       (display-ln
;;        (run* vz (conj (full-addero vx vy 1 0 1) (conso vx vy vz))))
      (and (equal? '(1 2 3)
                   (run* vx (element-of vx '(1 2 3))))
           (equal? '()
                   (run* vx (conj* `(,(== vx 6)
                                     ,(== vx vy)
                                     ,(== vy 5)))))
           (equal? '(0 1 2 3 4)
                   (run 5 vy (conj (== vy vx)
                                   (natural vx))))
           (equal? '()
                   (run* vx (conj (element-of vx '(1 2 3))
                                  (== vx 0))))
           (equal? '((() (s c h i s m))
                     ((s) (c h i s m))
                     ((s c) (h i s m))
                     ((s c h) (i s m))
                     ((s c h i) (s m))
                     ((s c h i s) (m))
                     ((s c h i s m) ()))
                   (run* vz (conj (== vz `(,vx ,vy))
                                  (appendo vx vy '(s c h i s m)))))
           (equal? '(2)
                   (run* vx (rembero vx '(1 2 3) '(1 3))))
           (equal? '(i)
                   (run* vx (common-element vx '(s c h i s m) '(a e i o u))))
           (equal? '(0 0 1 1 2 2 3 3 4 4)
                   (run 10 vy (conj (element-of vx '(1 2))
                                    (natural vy))))
           (equal? '(0 1 2 3 4 5 6 7 8 9)
                   (run 10 vy (conj-i (element-of vx '(1 2))
                                      (natural vy))))
           (equal? '(() (1 0 1) (0 0 1 1))
                   (map int->bits '(0 5 12)))
           (equal? '((1 . 0) (0 . 1))
                   (run* vz
                         (conj (full-addero vx vy 1 0 1)
                               (conso vx vy vz))))
           (equal? '()
                   (run* vz
                         (conj (full-addero vx vy 0 1 1)
                               (conso vx vy vz))))
           )))
  )
