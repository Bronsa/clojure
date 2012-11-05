(ns clojure.test-clojure.transients
  (:use clojure.test))

(deftest popping-off
  (testing "across a node boundary"
    (are [n] 
      (let [v (-> (range n) vec)]
        (= (subvec v 0 (- n 2)) (-> v transient pop! pop! persistent!)))
      33 (+ 32 (inc (* 32 32))) (+ 32 (inc (* 32 32 32)))))
  (testing "off the end"
    (is (thrown-with-msg? IllegalStateException #"Can't pop empty vector"
          (-> [] transient pop!))))
  (testing "copying array from a non-editable when put in tail position")
    (is (= 31 (let [pv (vec (range 34))]
                (-> pv transient pop! pop! pop! (conj! 42))
                (nth pv 31)))))

(defn- hash-obj [hash]
  (reify Object (hashCode [this] hash)))

(deftest dissocing
  (testing "dissocing colliding keys"
    (is (= [0 {}] (let [ks (concat (range 7) [(hash-obj 42) (hash-obj 42)])
                        m (zipmap ks ks)
                        dm (persistent! (reduce dissoc! (transient m) (keys m)))]
                    [(count dm) dm])))))

(deftest test-disj!
  (testing "disjoin multiple items in one call"
    (is (= #{5 20} (-> #{5 10 15 20} transient (disj! 10 15) persistent!)))))

(deftest empty-transient
  (is (= false (.contains (transient #{}) :bogus-key))))

(deftest ops-on-transient-maps
  (doseq [x [{:foo 1 :bar 2}
             (array-map :foo 1 :bar 2)
             (hash-map :foo 1 :bar 2)]]  ; no transient sorted maps
    (is (= (:foo x) (:foo (transient x))))
    (is (= (count x) (count (transient x))))
    (are [conj-args] (= (apply conj x conj-args)
                        (persistent!
                         (apply conj! (transient x) conj-args)))
         '()
         '({:bar -3 :baz 10})
         '({:bar -3 :baz 10} {:blub 0}))
    (are [assoc-args] (= (apply assoc x assoc-args)
                         (persistent!
                          (apply assoc! (transient x) assoc-args)))
         '()
         '(:blub 0)
         (mapcat (fn [i] [i (inc i)]) (range 1000)))
    (are [dissoc-args] (= (apply dissoc x dissoc-args)
                          (persistent!
                           (apply dissoc! (transient x) dissoc-args)))
         '()
         '(:foo)
         '(:foo :bar))))

(deftest ops-on-transient-vectors
  (let [x ["foo" "bar"]]
    (is (= "bar" ((transient x) 1)))
    (is (= 2 (count (transient x))))
    (are [conj-args] (= (apply conj x conj-args)
                        (persistent!
                         (apply conj! (transient x) conj-args)))
         '()
         '("baz")
         '("baz" "blub"))
    (are [assoc-args] (= (apply assoc x assoc-args)
                         (persistent!
                          (apply assoc! (transient x) assoc-args)))
         '()
         '(0 "blub")
         (mapcat (fn [i] [i (inc i)]) (range 1000)))
    (is (= (pop x)
           (persistent! (pop! (transient x)))))))

(deftest ops-on-transient-sets
  (doseq [x [#{"foo" "bar"} (hash-set "foo" "bar")]]  ; no transient sorted sets
    (is (= "bar" ((transient x) "bar")))
    (is (= 2 (count (transient x))))
    (are [conj-args] (= (apply conj x conj-args)
                        (persistent!
                         (apply conj! (transient x) conj-args)))
         '()
         '("baz")
         '("baz" "blub"))
    (are [disj-args] (= (apply disj x disj-args)
                        (persistent!
                         (apply disj! (transient x) disj-args)))
         '()
         '("bar")
         '("bar" "baz" "foo"))))
