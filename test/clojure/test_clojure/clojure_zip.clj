;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Frantisek Sodomka


(ns clojure.test-clojure.clojure-zip
  (:use clojure.test)
  (:require [clojure.zip :as zip]))


; zipper
;
; seq-zip
; vector-zip
; xml-zip
;
; node
; branch?
; children
; make-node
; path
; lefts
; rights
; down
; up
; root
; right
; rightmost
; left
; leftmost
;
; insert-left
; insert-right
; replace
; edit
; insert-child
; append-child
; next
; prev
; end?
; remove
; node-seq

(deftest node-seq-test
  (let [s '(((1 2) 3) (4 (5)))
        ns (zip/node-seq (zip/seq-zip s))]
    (is (= (map zip/node (remove zip/branch? ns))
           '(1 2 3 4 5)))
    (is (= (map zip/node (filter zip/branch? ns))
           '((((1 2) 3) (4 (5))) ((1 2) 3) (1 2) (4 (5)) (5))))))
