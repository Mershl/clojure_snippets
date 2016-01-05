(ns CljParser
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

(defn checkBlanksAfterBraces [str]
    (if (empty? str)
      true
      (let [[res1 in1] ((and-lazy (item \)) (item \space)) str)]
        (if (nil? res1)
          (checkBlanksAfterBraces (rest str))
          false
        )
      )
    )
)

(defn is-valid [str]
  (checkBlanksAfterBraces str)
)

(is-valid "Hello)Quack")