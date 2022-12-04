(ns advent-of-code-2022.day4
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(defn parse-pair [p] (apply range (map parse-int (clojure.string/split p #"-"))))

(defn count-contained-pairs [input-file]
  (let [pairs           (clojure.string/split-lines (slurp input-file))
        parse-pair      (fn [p]
                          (map parse-int (clojure.string/split p #"-")))

        fully-contains  (fn [p-large p-small]
                          (and
                            (<= (first p-large) (first p-small))
                            (<= (second p-small) (second p-large))))

        fully-contained (fn [p]
                          (let [p (map parse-pair (clojure.string/split p #","))
                                p (sort-by (fn [[x y]] (- y x))
                                           p)
                                [p-small p-large] p]

                            (fully-contains p-large p-small)))]

    (reduce (fn [c p]
              (if (fully-contained p)
                (inc c)
                c))
            0
            pairs)))

(deftest part1-testing

  (is (= 2 (count-contained-pairs "resources/day4-sample.txt")))
  (is (= 562 (count-contained-pairs "resources/day4.txt"))))