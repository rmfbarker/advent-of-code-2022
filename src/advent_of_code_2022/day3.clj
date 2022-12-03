(ns advent-of-code-2022.day3
  (:require [clojure.test :refer :all]))


(defn common-item [items]

  (let [[comp-1 comp-2] (split-at (/ (count items) 2) items)]
    (first (filter #(contains? (set comp-1) %) comp-2))))

(defn item-value [i]
  (if (< 96 (int i))
    (- (int i) 96)
    (- (int i) 38)))

(defn common-item-value [l]
  (-> l common-item item-value))

(defn sum-items [f]
  (reduce + (map
              common-item-value
              (clojure.string/split-lines (slurp f))))
  )

(deftest part-1

  (testing "that we parse correctly"

    (is (= \p (common-item "vJrwpWtwJgWrhcsFMMfFFhFp")))
    )

  (testing "item value"
    (is (= 1 (item-value \a)))
    (is (= 26 (item-value \z)))
    (is (= 27 (item-value \A)))
    (is (= 52 (item-value \Z)))
    )

  (testing "sum values"

    (is (= 157 (sum-items "resources/day3-sample.txt"))))
  (is (= 8053 (sum-items "resources/day3.txt"))))


(defn common-items [c1 c2]
  (filter (set c1) c2))

(defn badge [rucksacks]
  (first
    (reduce (fn [c1 c2]
              (common-items c1 c2))
            rucksacks))
  )

(defn part2 [input]
  (reduce + (map item-value (map badge (partition 3 (clojure.string/split-lines (slurp input)))))))

(deftest part2-test
  (testing "common item in group"
    (is (= \r (badge (clojure.string/split-lines "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\n\n"))))
    (is (= \Z (badge (clojure.string/split-lines "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"))))

    (is (= 2425 (part2 "resources/day3.txt")))
    )

  )