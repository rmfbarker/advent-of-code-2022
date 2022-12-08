(ns advent-of-code-2022.day6
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(defn find-marker [n data-stream]
  (+ n (count (take-while
                (fn [x] (not= (count x) (count (set x))))
                (partition n 1 data-stream)))))

(defn start-of-packet-marker [data-stream]
  (find-marker 4 data-stream))

(defn start-of-message-marker [data-stream]
  (find-marker 14 data-stream))

(defn part1 []
  (start-of-packet-marker
    (slurp "resources/day6.txt")))

(defn part2 []
  (start-of-message-marker
    (slurp "resources/day6.txt")))

(deftest test-solution
  (testing "part 1"
    (is (= 1175 (part1))))

  (testing "part 2"
    (is (= 3217 (part2)))))

