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

(library (define-constant)
  (export do-test)
  (import (rnrs))

  (define a #f)
  (define b 12)
  (define c (cons b 13))

  (define (do-test)
    (display (cons b c)) (newline)
    (and (not a) (eq? 12 b))))



