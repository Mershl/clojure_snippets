(ns SudokuSolverV0_5
  (:use clojure.test)
  (:use [clojure.string :only [join trim]]))

;;;; A variant of Peter Norvig's sudoku solver written in Clojure
;;;; See http://norvig.com/sudoku.html

(def grid1 "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..")
(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")

;; -------------------------
;; Utilities we need
;; -------------------------

(declare squares rows cols)

(def NOT-YET-IMPLEMENTED "Not yet implemented... ")

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
  "Display values as a 2D grid"
  [values]
  (let [width (inc (apply max (map (comp count values) #'squares)))
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
(def digits [1 2 3 4 5 6 7 8 9])
(def rows [:a :b :c :d :e :f :g :h :i])
(def cols [1 2 3 4 5 6 7 8 9])

;; furthermore, each element in the sudoku is a square, hence we
;; will have 81 squares in total which are called a1, a2, ... i8, i9
(def squares
  (for [r rows c cols] [r c])
)

;; this contains all possible units
;; we have 9 rows, 9 cols and 9 boxes - makes 27 in total
(def unitlist
  (let [allrows #(for [r rows] (for [c cols] [r c]))
        allcols #(for [c cols] (for [r rows] [r c]))
        allboxes #(for [r rows] (take 3 (partition 3 rows)))]
    ;(concat (allrows) (allcols) (allboxes))
    (allboxes)
  )
)

;; a map which holds all units a square is member of
;; each square is a member of exactly 3 units (one row, one col, one box)
(def units NOT-YET-IMPLEMENTED)

;; peers are all squares that share a unit with a specific square - we again use a map here
;; each square should have 20 peers
(def peers NOT-YET-IMPLEMENTED)

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

;; -------------------------
;; Set up Sudoku
;; -------------------------

;; we set up our Sudoku by reading data from a string representing
;; the initial state. Each char represents a square and contains either
;; 1-9 for specified values or 0 or . for empty squares.

;; will read a string representation and return a map {squares digits}
(defn grid-values [grid] NOT-YET-IMPLEMENTED)

;; returns a map {square digits} with all possible values
(defn init-values [] NOT-YET-IMPLEMENTED)

(is (= (grid-values grid1)
       {[:a 1] nil, [:a 2] nil, [:a 3] 3, [:a 4] nil, [:a 5] 2, [:a 6] nil, [:a 7] 6, [:a 8] nil, [:a 9] nil,
        [:b 1] 9, [:b 2] nil, [:b 3] nil, [:b 4] 3, [:b 5] nil, [:b 6] 5, [:b 7] nil, [:b 8] nil, [:b 9] 1,
        [:c 1] nil, [:c 2] nil, [:c 3] 1, [:c 4] 8, [:c 5] nil, [:c 6] 6, [:c 7] 4, [:c 8] nil, [:c 9] nil,
        [:d 1] nil, [:d 2] nil, [:d 3] 8, [:d 4] 1, [:d 5] nil, [:d 6] 2, [:d 7] 9, [:d 8] nil, [:d 9] nil,
        [:e 1] 7, [:e 2] nil, [:e 3] nil, [:e 4] nil, [:e 5] nil, [:e 6] nil, [:e 7] nil, [:e 8] nil, [:e 9] 8,
        [:f 1] nil, [:f 2] nil, [:f 3] 6, [:f 4] 7, [:f 5] nil, [:f 6] 8, [:f 7] 2, [:f 8] nil, [:f 9] nil,
        [:g 1] nil, [:g 2] nil, [:g 3] 2, [:g 4] 6, [:g 5] nil, [:g 6] 9, [:g 7] 5, [:g 8] nil, [:g 9] nil,
        [:h 1] 8, [:h 2] nil, [:h 3] nil, [:h 4] 2, [:h 5] nil, [:h 6] 3, [:h 7] nil, [:h 8] nil, [:h 9] 9,
        [:i 1] nil, [:i 2] nil, [:i 3] 5, [:i 4] nil, [:i 5] 1, [:i 6] nil, [:i 7] 3, [:i 8] nil, [:i 9] nil}))


;; ---------------------------------------
;; Constraint Propagation
;; ---------------------------------------

;; entry point for constraint propagation,
;; gets the string represantation of the grid as param
(defn parse-grid [grid] NOT-YET-IMPLEMENTED)

;; assigns value d to square s
;; does so by eliminating all other values from this square
;; Param values is the current solution
(defn assign [values s d] NOT-YET-IMPLEMENTED)

;; eliminate value d from square s
;; use the strategies for constraint propagation here
;; Param values is the current solution
(defn eliminate [values s d] NOT-YET-IMPLEMENTED)

;; -------------------------
;; Search
;; -------------------------

;; search for suitable values by using constraint propagation
;; Param values is the current solution
(defn search [values] NOT-YET-IMPLEMENTED)

;; -------------------------
;; Solve Sudoku
;; -------------------------

(defn solve [grid] NOT-YET-IMPLEMENTED)