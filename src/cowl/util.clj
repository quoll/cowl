(ns cowl.util
  {:doc "Common utilities"
   :author "Paula Gearon"})

(defmacro import-fn
  "Imports a function from another namespace. Loosely copied from Potemkin by Zachary Tellman."
  [s]
  (let [f (resolve s)
        n (symbol (name s))
        m (meta f)
        p (:protocol m)]
    (when-not f (throw (ex-info (str "Uknown symbol: " s) {:symbol s})))
    ;; alter-meta! after creation, so symbols in arglists are not dereferenced
    (if p
      `(let [f# (resolve '~s)]
         (def ~(with-meta n {:protocol p}) (deref f#))
         (alter-meta! (var ~n) merge (dissoc (meta f#) :name)))
      `(let [f# (resolve '~s)]
         (def ~n (deref f#))
         (alter-meta! (var ~n) merge (dissoc (meta f#) :name))))))
