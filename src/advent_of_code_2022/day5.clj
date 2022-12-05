(ns advent-of-code-2022.day5
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def input (parse-lines "resources/day5.txt"))

(def raw-columns (take-while #((complement clojure.string/includes?) % "1") input))

(def rows (map
            (fn [row]
              (map
                (fn [cell]
                  (clojure.string/trim (apply str cell))) row))
            (map #(partition-all 4 %) raw-columns)))

(def cols (vec
            (map
              (fn [col]
                (filter seq col))
              (apply map vector rows))))

(def instructions
  (let [raw-instructions (rest (drop-while
                                 #((complement clojure.string/blank?) %)
                                 input))
        digit-instrs     (map #(map
                                 parse-int
                                 (re-seq #"\d+" %)) raw-instructions)]
    (map (fn [[move from to]]
           (vector move (dec from) (dec to)))
         digit-instrs)))

(defn do-move-9000
  "returns the new 'from' column"
  [cols no from to]
  (concat (reverse (take no
                         (nth cols from)))
          (nth cols to)))

(defn do-move-9001
  "returns the new 'from' column"
  [cols no from to]
  (concat (take no
                (nth cols from))
          (nth cols to)))

(defn step [move-fn cols instr]
  (let [[move from to] instr]
    (-> cols
        (assoc to (move-fn cols move from to))
        (assoc from (drop move (nth cols from))))))

(defn top-crates [state]
  (let [top-crates (map first state)]
    (apply str
           (re-seq
             #"\w"
             (apply str top-crates)))))

(defn iterate-crates [step-fn]
  (let [final-state (reduce step-fn cols instructions)]
    (top-crates final-state)))

(defn compute-part1 []
  (iterate-crates (partial step do-move-9000)))

(defn compute-part2 []
  (iterate-crates (partial step do-move-9001)))

(deftest part-1
  (is (= "TGWSMRBPN" (compute-part1))))

(deftest part-2
  (is (= "TZLTLWRNF" (compute-part2))))