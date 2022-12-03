(ns advent-of-code-2022.day1
  (:require [clojure.test :refer :all]))

(defn read-input [input-file]
  (clojure.string/split (slurp input-file)
                        #"\n\n"))

(defn parse-int [s] (Integer/parseInt s))

(defn elves-sorted-by-cals [elves]
  (let [
        count-cals (fn [elf] (apply + (map parse-int (clojure.string/split-lines elf))))
        elf-cals   (map-indexed vector (map count-cals elves))]

    (sort-by second elf-cals)))

(defn most-calorific-elf [input-file]
  (let [elves (read-input input-file)]

    (second (last (elves-sorted-by-cals elves)))))

(defn calories-carried-by-top3 [input-file]
  (let [elves (read-input input-file)]

    (reduce + (map second (take 3 (reverse (elves-sorted-by-cals elves)))))))

(deftest part1-checks
  (is (= 24000 (most-calorific-elf "resources/day1-sample.txt")))
  (is (= 70369 (most-calorific-elf "resources/day1.txt"))))

(deftest part2-checks
  (is (= 45000 (calories-carried-by-top3 "resources/day1-sample.txt"))))
