(ns advent-of-code-2022.day4
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(defn parse-pair [p]
  (let [[a b] (map parse-int (clojure.string/split p #"-"))]
    (range a (inc b))))

(defn contained? [p-large p-small]
  (every? (set p-large) p-small))

(defn overlaps? [p1 p2]
  (some (set p1) p2))

(defn pair-matches? [match-fn p]
  (let [p (map parse-pair (clojure.string/split p #","))
        [p-small p-large] (sort-by count p)]

    (match-fn p-large p-small)))

(def overlapping-pred (partial pair-matches? overlaps?))
(def contained-pred (partial pair-matches? contained?))

(defn sum-pairs [input-file match-fn]
  (let [pairs (parse-lines input-file)]

    (count
      (filter (fn [p]
                (match-fn p))
              pairs))))

(deftest pair-testing
  (is (= [3] (parse-pair "3-3")))
  (is (= 2 (sum-pairs "resources/day4-sample.txt" contained-pred)))
  (is (= 4 (sum-pairs "resources/day4-sample.txt" overlapping-pred)))
  (is (= 562 (sum-pairs "resources/day4.txt" contained-pred)))
  (is (= 924 (sum-pairs "resources/day4.txt" overlapping-pred))))
