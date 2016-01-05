;;;; A variant of Peter Norvig's sudoku solver written in Clojure
;;;; See http://norvig.com/sudoku.html

(ns SudokuSolverV1_5
  (:use clojure.test)
  (:use [clojure.string :only [join trim]]))


(def easy "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..")
(def medium "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard ".....6....59.....82....8....45........3........6..3.54...325..6..................")

;; -------------------------
;; Utilities we need
;; -------------------------

(declare squares rows cols assign eliminate)

;; return true if character c is a digit
(defn digit? [c]
  (Character/isDigit c))

;; convert a character into a Long
;; i.e. for c = \5 it will return 5
;; does not work for letters!
(defn digit [c]
  (Character/digit c 10))

;; display a 2D grid of values
(defn display
  [values]
  (let [width (inc (apply max (map (comp count values) squares)))
        line (join \+ (repeat 3 (join (repeat (* 3 width) \-))))]
    (doseq [r rows]
      (println (join (for [c cols]
                       (format (str "%-" width "s%s")
                               (join (values [r c]))
                               (if (#{3 6} c) "|" "")))))
      (when (#{:c :f} r) (println line)))))

;; -------------------------
;; Data structures
;; -------------------------

;; we need some basic stuff to store data
;; allowed values are only numbers from 1 to 10
;; sudoku enthusiasts define rows with the letters a - i
;; and columns with the digits 1 - 9
(def digits (set (range 1 10)))
(def rows [:a :b :c :d :e :f :g :h :i])
(def cols (into [] (range 1 10)))

;; furthermore, each element in the sudoku is a square, hence we
;; will have 81 squares in total which are called a1, a2, ... i8, i9
(def squares (for [r rows c cols] [r c]))

;; this contains all possible units
;; we have 9 rows, 9 cols and 9 boxes - makes 27 in total
(def unitlist (concat (for [c cols] (for [r rows] [r c])) ; 9 cols
                      (for [r rows] (for [c cols] [r c])) ; 9 rows
                      (for [rs (partition 3 rows) cs (partition 3 cols)] ; 9 box
                        (for [r rs c cs] [r c]))))

;; a map which holds all units a square is member of
;; each square is a member of exactly 3 units (one row, one col, one box)
(def units (into {} (for [s squares]
                      [s (for [u unitlist :when (some #{s} u)] u)])))

;; peers are all squares that share a unit with a specific square - we again use a map here
;; each square should have 20 peers
(def peers (into {} (for [s squares]
                      [s (disj (reduce into #{} (units s)) s)])))

(defn grid-values [data]
  (let [chdata (for [x data] (if (= x \.) nil x))]
    (zipmap squares chdata)
  )
)

;; let's test our datastructures first
(is (= 81 (count squares)))
(is (every? #(= 3 (count (units %))) squares))
(is (every? #(= 20 (count (peers %))) squares))

(is (= 27 (count unitlist)))
(is (=  unitlist
       '(([:a 1] [:b 1] [:c 1] [:d 1] [:e 1] [:f 1] [:g 1] [:h 1] [:i 1])
             ([:a 2] [:b 2] [:c 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2])
             ([:a 3] [:b 3] [:c 3] [:d 3] [:e 3] [:f 3] [:g 3] [:h 3] [:i 3])
             ([:a 4] [:b 4] [:c 4] [:d 4] [:e 4] [:f 4] [:g 4] [:h 4] [:i 4])
             ([:a 5] [:b 5] [:c 5] [:d 5] [:e 5] [:f 5] [:g 5] [:h 5] [:i 5])
             ([:a 6] [:b 6] [:c 6] [:d 6] [:e 6] [:f 6] [:g 6] [:h 6] [:i 6])
             ([:a 7] [:b 7] [:c 7] [:d 7] [:e 7] [:f 7] [:g 7] [:h 7] [:i 7])
             ([:a 8] [:b 8] [:c 8] [:d 8] [:e 8] [:f 8] [:g 8] [:h 8] [:i 8])
             ([:a 9] [:b 9] [:c 9] [:d 9] [:e 9] [:f 9] [:g 9] [:h 9] [:i 9])
             ([:a 1] [:a 2] [:a 3] [:a 4] [:a 5] [:a 6] [:a 7] [:a 8] [:a 9])
             ([:b 1] [:b 2] [:b 3] [:b 4] [:b 5] [:b 6] [:b 7] [:b 8] [:b 9])
             ([:c 1] [:c 2] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9])
             ([:d 1] [:d 2] [:d 3] [:d 4] [:d 5] [:d 6] [:d 7] [:d 8] [:d 9])
             ([:e 1] [:e 2] [:e 3] [:e 4] [:e 5] [:e 6] [:e 7] [:e 8] [:e 9])
             ([:f 1] [:f 2] [:f 3] [:f 4] [:f 5] [:f 6] [:f 7] [:f 8] [:f 9])
             ([:g 1] [:g 2] [:g 3] [:g 4] [:g 5] [:g 6] [:g 7] [:g 8] [:g 9])
             ([:h 1] [:h 2] [:h 3] [:h 4] [:h 5] [:h 6] [:h 7] [:h 8] [:h 9])
             ([:i 1] [:i 2] [:i 3] [:i 4] [:i 5] [:i 6] [:i 7] [:i 8] [:i 9])
             ([:a 1] [:a 2] [:a 3] [:b 1] [:b 2] [:b 3] [:c 1] [:c 2] [:c 3])
             ([:a 4] [:a 5] [:a 6] [:b 4] [:b 5] [:b 6] [:c 4] [:c 5] [:c 6])
             ([:a 7] [:a 8] [:a 9] [:b 7] [:b 8] [:b 9] [:c 7] [:c 8] [:c 9])
             ([:d 1] [:d 2] [:d 3] [:e 1] [:e 2] [:e 3] [:f 1] [:f 2] [:f 3])
             ([:d 4] [:d 5] [:d 6] [:e 4] [:e 5] [:e 6] [:f 4] [:f 5] [:f 6])
             ([:d 7] [:d 8] [:d 9] [:e 7] [:e 8] [:e 9] [:f 7] [:f 8] [:f 9])
             ([:g 1] [:g 2] [:g 3] [:h 1] [:h 2] [:h 3] [:i 1] [:i 2] [:i 3])
             ([:g 4] [:g 5] [:g 6] [:h 4] [:h 5] [:h 6] [:i 4] [:i 5] [:i 6])
             ([:g 7] [:g 8] [:g 9] [:h 7] [:h 8] [:h 9] [:i 7] [:i 8] [:i 9]))))

(is (= (units [:c 2])
       [[[:a 2] [:b 2] [:c 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2]]
        [[:c 1] [:c 2] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9]]
        [[:a 1] [:a 2] [:a 3] [:b 1] [:b 2] [:b 3] [:c 1] [:c 2] [:c 3]]]))

(is (= (peers [:c 2])
       #{[:a 2] [:b 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2]
         [:c 1] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9]
         [:a 1] [:a 3] [:b 1] [:b 3]}))