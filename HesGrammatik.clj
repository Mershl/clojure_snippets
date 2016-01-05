(ns .HesGrammatik
    (:refer-clojure :exlude (and or not drop hash))
  (:use [clojure.string :only (upper-case)])
  (:use clojure.test))


;; -------------------------------
;; A couple of different parsers
;; -------------------------------

(defn success [in] [[] in])
(defn fail [in] [nil in])

(defn item [itm]
  (fn [in]
    (if (empty? in)
      (fail in)
      (if (= (first in) itm)
        [[itm] (rest in)]
        (fail in)))))

(defn or [& parsers]
  (fn [in]
    (let [res (for [r (map #(% in) parsers) :when (first r)] r)]
      (if (empty? res) (fail in) (first res)))))

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
        (fail in)))))

(defn and-lazy [& parsers]
  (fn [in]
    (let [res (take-while #(first %) (pipe parsers in))]
      (if (empty? res)
        (fail in)
        [(reduce concat (map first res)) (second (last res))]))))

(defn optional [parser] (or parser success))

(defn more [parser] (apply and-lazy (cycle [parser])))

(defn many [parser] (optional (more parser)))

;; ------------------------------
;; Our Clojure Expression Parser
;; ------------------------------

(declare expr)

(defn or-parser [s]
  (apply or (map item s)))

(def alphabet (or-parser "abcdefghijklmnopqrstuvwxyz"))
(def digits (or-parser "0123456789"))
(def nnum (more digits))
(def symb (and alphabet (many (or alphabet nnum))))
(def blank (item \space))
(def llist (and (item \()
                (optional (and #'expr (many (and blank #'expr))))
                (item \))))

;; extended by allowing optional blanks
;(def llist (and (item \()
;                (optional blank)
;                (optional (and #'expr (many (and blank #'expr))))
;                (optional blank)
;                (item \))))

;; extended by allowing unlimited blanks and newlines
;(def blank (or (item \space) (item \newline)))
;(def llist (and (item \()
;                (many blank)
;                (optional (and #'expr (many (and (more blank) #'expr))))
;                (many blank)
;                (item \))))

(def expr (or llist symb nnum))

;; entry point, receives an expression as a string
;; valid parsings return with a vector [[-parsed stuff-] ()]
;; hence we can check by looking for an empty second part
(defn is-valid [e]
  (empty? (second (expr e))))

(is (is-valid "(def x 42)"))
(is (is-valid "(pow 2 (pow 2 3))"))

(is (not (is-valid "(def x 42")))
(is (not (is-valid "(pow 2(pow 2 3))")))
