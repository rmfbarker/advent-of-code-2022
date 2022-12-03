(ns advent-of-code-2022.day2
  (:require [clojure.test :refer :all]))

(def player-1 {"A" :rock "B" :paper "C" :scissors})
(def player-2 {"X" :rock "Y" :paper "Z" :scissors})

(def hand-value {:rock 1 :paper 2 :scissors 3})

(defn match-result [a b]
  (cond
    (= a b) :draw

    (or
      (= [:rock :scissors] [b a])
      (= [:scissors :paper] [b a])
      (= [:paper :rock] [b a]))
    :win

    :else
    :lose))

(defn game-points-result [res]
  ({:win 6 :draw 3 :lose 0} res))

(defn game-result [a b]
  (game-points-result (match-result a b)))

(defn parse-round [s]
  (let [[p1 p2] (clojure.string/split s #" ")]
    (+ (game-result (player-1 p1) (player-2 p2)) (hand-value (player-2 p2)))
    )
  )

(defn parse-game-results [f]
  (reduce + (map parse-round (clojure.string/split-lines (slurp f)))))

(deftest part-1
  (is (= 6 (game-result :rock :paper)))
  (is (= 3 (game-result :paper :paper)))
  (is (= 0 (game-result :paper :rock)))

  (is (= 8 (parse-round "A Y")))
  (is (= 1 (parse-round "B X")))
  (is (= 6 (parse-round "C Z")))

  (is (= 15 (parse-game-results "resources/day2-sample.txt")))
  (is (= 10718 (parse-game-results "resources/day2.txt")))

  #_(is (= 70369 (most-calorific-elf "resources/day1.txt"))))


;; X means you need to lose,
;; Y means you need to end the round in a draw
;; Z means you need to win
(defn desired-result [s]
  (get {"X" :lose "Y" :draw "Z" :win} s))

(defn move-to-make [p1 result]
  (condp = result
    :draw p1
    :lose ({:rock :scissors :paper :rock :scissors :paper} p1)
    :win ({:scissors :rock :rock :paper :paper :scissors} p1)))

(deftest choosing-moves

  (is (= :rock (move-to-make :rock :draw)))
  (is (= :paper (move-to-make :rock :win)))
  (is (= :scissors (move-to-make :rock :lose)))

  (is (= :rock (move-to-make :scissors :win)))
  (is (= :paper (move-to-make :scissors :lose)))
  (is (= :scissors (move-to-make :scissors :draw)))

  (is (= :scissors (move-to-make :paper :win)))
  (is (= :rock (move-to-make :paper :lose)))
  (is (= :paper (move-to-make :paper :draw))))

(defn parse-round-2 [s]

  (let [[p1 p2] (clojure.string/split s #" ")
        p1 (player-1 p1)
        result (desired-result p2)]

    (+ (game-points-result result) (hand-value (move-to-make p1 result)))))

(defn total-match-results [input]
  (reduce + (map parse-round-2 (clojure.string/split-lines (slurp input)))))

(deftest processing-moves

  (is (= 4 (parse-round-2 "A Y")))
  (is (= 1 (parse-round-2 "B X")))
  (is (= 7 (parse-round-2 "C Z")))

  (is (= 12 (total-match-results "resources/day2-sample.txt")))
  (is (= 12 (total-match-results "resources/day2.txt")))

  )