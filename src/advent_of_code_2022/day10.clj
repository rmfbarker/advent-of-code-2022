(ns advent-of-code-2022.day10
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def sample-file "resources/day10-sample.txt")
(def input-file "resources/day10.txt")

(defn cycle-states [file-path]
  (reduce
    (fn [states instr]
      (let [curr (peek states)]
        (conj states
              (if (= instr "noop")
                (update curr :cycle inc)
                (let [addx (parse-int (second (clojure.string/split instr #" ")))]
                  (-> curr
                      (update :cycle + 2)
                      (update :register + addx)))))))
    [{:cycle 0 :register 1}]
    (parse-lines file-path)))

(def key-cycles [20 60 100 140 180 220])

(defn part-1 [file-path]
  (let [states  (cycle-states file-path)
        signals (reduce
                  (fn [agg key-cycle]
                    (let [{:keys [register]} (last
                                               (take-while
                                                 (fn [{:keys [cycle]}]
                                                   (< cycle key-cycle))
                                                 states))]
                      (conj agg (* register key-cycle))))
                  []
                  key-cycles)]
    (reduce + signals)))

(deftest test-day-10
  (is (= 13140 (part-1 sample-file)))
  (is (= 14040 (part-1 input-file)))
  )
