(ns advent-of-code-2022.day8
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def input-file "resources/day8-sample.txt")
(def input-file "resources/day8.txt")

(def input (mapv #(mapv parse-int %)
                 (parse-lines input-file)))

(def y-length (count input))
(def x-length (count (first input)))


(defn get-col [col] (map (fn [row] (get row col)) input))

(defn get-tree [x y] (get (get input y) x))

;30373
;25512
;65332
;33549
;35390

; emit the row
(def visible-trees
  (count
    (filter identity
            (for [x (range x-length) y (range y-length)
                  :let [tree        (get-tree x y)

                        row-left    (take x (get input y))
                        row-right   (drop (inc x) (get input y))
                        col-up      (take y (get-col x))
                        col-down    (drop (inc y) (get-col x))

                        is-shorter? #(< % tree)
                        ]]
              (do
                ;(println [x y] tree "row left" row-left "visible from  left " (every? is-shorter? row-left))
                ;
                ;(println [x y] tree "row right" row-right "visible from right " (every? is-shorter? row-right))
                ;;
                ;(println [x y] "col up" col-up "visible from top" (every? is-shorter? col-up))
                ;;
                ;(println [x y] "col down" col-down "visible from bottom" (every? is-shorter? col-down))
                ;[x y]

                (or
                  (every? is-shorter? row-left)
                  (every? is-shorter? row-right)
                  (every? is-shorter? col-up)
                  (every? is-shorter? col-down)))))))

(deftest part-1-test (is (= 1688 visible-trees)))