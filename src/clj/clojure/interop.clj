;;   Copyright Nicola Mometto, Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Interop utility macros to define custom classes."
      :author "Nicola Mometto"}
  clojure.interop)

(def ^:private validate-fields @#'clojure.core/validate-fields)
(def ^:private parse-opts+specs @#'clojure.core/parse-opts+specs)
(def ^:private parse-opts @#'clojure.core/parse-opts)
(def ^:private build-positional-factory @#'clojure.core/build-positional-factory)

(defn- emit-defclass*
  [name fields interfaces methods opts]
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))
        interfaces (conj interfaces 'clojure.lang.IType)]
    `(deftype* ~name ~classname ~fields
       :implements ~interfaces
       ~@(mapcat identity opts)
       ~@methods)))

(defmacro defclass
  "(defclass name [fields*] options* super-class super-args specs*)

   Like clojure.core/deftype but can extend a concrete class, override
   and invoke public and protected methods defined in the super class or
   one of its base classes and access/set! public and protected fields of
   those.

   super-args is a (possibly empty) vector of arguments to the superclass
   constructor

   It is possible to invoke a super method while overriding it, by type-hinting
   the parameter `this` as the super class: (. ^SuperClass this method args*)"
  {:arglists '([name [& fields] & opts+specs])}
  [name fields & opts+specs]
  (validate-fields fields name)
  (let [[opts [super-class ctor & specs]] (parse-opts opts+specs)
        opts+specs (concat (mapcat identity opts) '(Object) specs)
        [interfaces methods opts] (parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." name))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        [field-args over] (split-at 20 fields)
        opts (assoc opts :super-class super-class :ctor-args ctor)]
    `(let []
       ~(emit-defclass* name (vec hinted-fields) (vec interfaces) methods opts)
       (import ~classname)
       ~(build-positional-factory name classname fields)
       ~classname)))

(defmacro extend-class
  "(extend-class options* super-class super-args specs*)

   Like clojure.core/reify but can extend a concrete class, override
   and invoke public and protected methods defined in the super class or
   one of its base classes and access/set! public and protected fields of
   those.

   Unlike clojure.core/reify, doesn't automatically implement IObj.

   super-args is a (possibly empty) vector of arguments to the superclass
   constructor

   It is possible to invoke a super method while overriding it, by type-hinting
   the parameter `this` as the super class: (. ^SuperClass this method args*)"
  [& opts+specs]
  (let [[opts [super-class ctor & specs]] (parse-opts opts+specs)
        opts+specs (concat (mapcat identity opts) '(Object) specs)
        [interfaces methods opts] (parse-opts+specs opts+specs)]
    `(reify* ~interfaces
             :super-class ~super-class
             :ctor-args ~ctor
             ~@methods)))
