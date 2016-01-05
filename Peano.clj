(ns pno
    (:refer-clojure
     :exclude (zero? == + - * / mod pos? neg? inc dec))
    (:use clojure.test))

(defn zero? [p] (= () p))
(defn- is-peano? [[fst snd :as p]]
   (or (= fst 'succ) (= fst 'pred) (zero? p)))

(defn succ [[fst snd :as p]]
  {:pre [(is-peano? p)]}
  (if (= fst 'pred) snd (list 'succ p)))
(defn pred [[fst snd :as p]]
  {:pre [(is-peano? p)]}
  (if (= fst 'succ) snd (list 'pred p)))

(defn pos? [p] (= (first p) 'succ))
(defn neg? [p] (= (first p) 'pred))

(defn peano
  ([n] {:pre [(integer? n)]} (peano n ()))
  ([n p]
    (cond
      (clojure.core/zero? n) p
      (clojure.core/pos?  n) (recur (clojure.core/dec n) (succ p))
      :else                  (recur (clojure.core/inc n) (pred p)))))
(defn decimal
  ([p] (decimal p 0))
  ([[fst snd :as p] n]
    {:pre [(is-peano? p)]}
    (cond
      (= fst 'pred) (recur snd (clojure.core/dec n))
      (= fst 'succ) (recur snd (clojure.core/inc n))
      :else         n)))

(defn + [p1 [fst snd :as p2]]
  (if (zero? p2) p1 (recur (eval (list fst p1)) snd)))

(defn *
  ([p1 p2] (* p1 p2 (peano 0)))
  ([p1 p2 res]
    (if (zero? p2) res
      (if (pos? p2)
        (recur (p1 (- p2 (peano 1)) (+ res p1)))
        (recur (p1 (+ p2 (peano 1)) (- res p1)))))))

;(neg? (succ (pred (succ ()))))
(+ '(pred (pred())) '(succ (succ (succ ()))))
(testor '"eins" '"zwei" ())