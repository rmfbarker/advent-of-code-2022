(ns advent-of-code-2022.day10
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def sample-file "resources/day10-sample.txt")
(def input-file "resources/day10.txt")

(defn parse-add-instr [instr]
  (-> instr
      (clojure.string/split #" ")
      second
      parse-int))

(defn do-addx [states instr]
  (let [curr (peek states)
        addx (parse-add-instr instr)]
    (conj states
          (update curr :cycle inc)
          (-> curr
              (update :cycle + 2)
              (update :register + addx)))))

(defn do-noop [states]
  (let [curr (peek states)
        next (update curr :cycle inc)]
    (conj states next)))

(defn cycle-states [file-path]
  (reduce
    (fn [states instr]
      (if (= instr "noop")
        (do-noop states)
        (do-addx states instr)))
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

(defn part-2 [file-path]
  (let [states (cycle-states file-path)]
   (loop [cycle 1]
     (when (< cycle (count states))
       (let [start-state (nth states (dec cycle))
             end-state   (nth states cycle)
             reg         (:register start-state)]
         #_(println "cycle" cycle "cursor pos" (dec cycle) "at start" start-state "at end" end-state
                    "crt" (if (<= reg cycle (+ 2 reg)) "#" "."))
         (print (if (<= reg (mod cycle 40) (+ 2 reg)) "#" "."))
         (if (zero? (mod cycle 40)) (println))
         (recur (inc cycle)))))))

(comment
  (part-2 sample-file)

;;
;;####..##...##....##.####...##.####.#....
  ;...#.#..#.#..#....#....#....#.#....#....
  ;..#..#....#.......#...#.....#.###..#....
  ;.#...#.##.#.......#..#......#.#....#....
  ;#....#..#.#..#.#..#.#....#..#.#....#....
  ;####..###..##...##..####..##..#....####.
  (part-2 input-file))