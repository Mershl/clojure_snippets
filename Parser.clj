(ns Parser
    (:refer-clojure :exclude (and or not drop))
    (:use clojure.test))

(defn item [itm]
  (fn [in]
    (if (empty? in)
      [nil in]
      (if (= (first in) itm)
        [[itm] (rest in)]
        [nil in]))))

(defn success [in] [[] in])
(defn fail [in] [nil in])

;(defn or [& parsers]
;   (fn [in]
;     (loop [ps parsers]
;       (if (empty? ps)
;         [nil in]
;         (let [[res1 in1] ((first ps) in)]
;           (if res1 [res1 in1] (recur (rest ps))))))))

(defn or [& parsers]
  (fn [in]
    (let [res (filter #(first %) (map #(% in) parsers))]
      (if (empty? res) [nil in] (first res)))))

(defn- pipe [parsers in]
  (lazy-seq
    (if (empty? parsers)
      ()
      (let [[res1 in1 :as res] ((first parsers) in)]
        (cons res (pipe (rest parsers) in1))))))

(defn and [& parsers]
  (fn [in]
    (let [res (pipe parsers in)]
      (if (every? #(first %) res)
        [(reduce concat (map first res)) (second (last res))]
        [nil in]))))

(defn and-lazy [& parsers]
  (fn [in]
    (let [res (take-while #(first %) (pipe parsers in))]
      (if (empty? res)
        [nil in]
        [(reduce concat (map first res)) (second (last res))]))))

 (defn optional [parser] (or parser success)) ; 1..0
 (defn more [parser] (apply and-lazy (cycle [parser]))) ; *..1

;((or (item 1) (item 2)) [2 3])
((and-lazy (item 1) (item 2)) [1 2 3])