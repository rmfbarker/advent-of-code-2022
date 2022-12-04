(ns advent-of-code-2022.day4
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(defn parse-pair [p]
  (let [[a b] (map parse-int (clojure.string/split p #"-"))]
    (range a (inc b))))

(def fully-contains (fn [p-large p-small]
                      (every? (set p-large) p-small)))

(def overlaps? (fn [p1 p2]
                 (some (set p1) p2)))

(def fully-contained (fn [p match-fn]
                       (let [p (map parse-pair (clojure.string/split p #","))
                             p (sort-by count p)
                             [p-small p-large] p]

                         (match-fn p-large p-small))))

(defn count-contained-pairs [input-file match-fn]
  (let [pairs (clojure.string/split-lines (slurp input-file))]

    (reduce (fn [c p]
              (if (fully-contained p match-fn)
                (inc c)
                c))
            0
            pairs)))

(deftest pair-testing
  (is (= [3] (parse-pair "3-3")))
  (is (= 2 (count-contained-pairs "resources/day4-sample.txt" fully-contains)))
  (is (= 4 (count-contained-pairs "resources/day4-sample.txt" overlaps?)))
  (is (= 562 (count-contained-pairs "resources/day4.txt" fully-contains)))
  (is (= 924 (count-contained-pairs "resources/day4.txt" overlaps?))))
