(ns advent-of-code-2022.day5
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def input (parse-lines "resources/day5.txt"))

(def raw-columns (take-while #((complement clojure.string/includes?) % "1") input))

(def cols (-> (last raw-columns)
              (clojure.string/replace "[" "")
              (clojure.string/replace "]" "")
              (clojure.string/split #" ")))

(def no-cols (count (re-seq #"\w" (last raw-columns))))

(def rows (map #(partition-all 4 %) raw-columns))

(def rows (map
            (fn [row]
              (map
                (fn [cell]
                  (clojure.string/trim (apply str cell))) row))
            rows))

(def cols (apply map vector rows))

(def cols (map
            (fn [col]
              (filter seq col))
            cols))

(def instructions
  (let [raw-instructions (rest (drop-while
                                 #((complement clojure.string/blank?) %)
                                 input))
        digit-instrs     (map #(map
                                 parse-int
                                 (re-seq #"\d+" %)) raw-instructions)]
    digit-instrs))

(defn do-move
  "returns the new 'from' column"
  [cols no from to]
  (concat (reverse (take no
                         (nth cols from)))
          (nth cols to)))

(defn step [cols instr]
  (let [[move from to] instr
        from (dec from) to (dec to)]
    (-> cols
        (assoc to (do-move cols move from to))
        (assoc from (drop move (nth cols from))))))

(defn answer []
  (let [final-state (reduce step (vec cols) instructions)
        top-crates  (map first final-state)]
    (apply str
           (re-seq
             #"\w"
             (apply str top-crates))))
  )

(deftest part-1
  (is (= "TGWSMRBPN" (answer))))