(ns advent-of-code-2022.day8
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

;(def input-file "resources/day8-sample.txt")
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

                        is-shorter? #(< % tree)]]
              (or
                (every? is-shorter? row-left)
                (every? is-shorter? row-right)
                (every? is-shorter? col-up)
                (every? is-shorter? col-down))))))

(deftest part-1-test (is (= 1688 visible-trees)))


(def part2
  (apply max
         (for [x (range x-length) y (range y-length)
               :let [tree          (get-tree x y)

                     row-left      (take x (get input y))
                     row-right     (drop (inc x) (get input y))
                     col-up        (take y (get-col x))
                     col-down      (drop (inc y) (get-col x))

                     trees-visible (fn [trees] (reduce (fn [acc t]
                                                         (if (<= tree t)
                                                           (reduced (inc acc))
                                                           (inc acc))) 0 trees))

                     trees-left    (trees-visible (reverse row-left))
                     trees-right   (trees-visible row-right)
                     trees-up      (trees-visible (reverse col-up))
                     trees-down    (trees-visible col-down)
                     ]]
           (* trees-left trees-right trees-up trees-down))))

(deftest part-1-test (is (= 410400 part2)))
