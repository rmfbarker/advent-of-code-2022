(ns advent-of-code-2022.day9
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def sample-file "resources/day9-sample.txt")
(def input-file "resources/day9.txt")

(defn moves [file-path]
  (map
    (fn [l]
      (let [move (clojure.string/split l #" ")]
        [(first move) (parse-int (second move))]))
    (parse-lines file-path)))

(defn do-move [dir pos]
  (let [[x y] pos]
    (condp = dir
      "R" [(inc x) y]
      "L" [(dec x) y]
      "U" [x (inc y)]
      "D" [x (dec y)])))

(defn adjacent? [head tail]
  (let [[xh yh] head
        [xt yt] tail]
    (and
      (<= (abs (- xh xt)) 1)
      (<= (abs (- yh yt)) 1))))

(defn same-col [head tail]
  (= (first head) (first tail)))

(defn same-row [head tail]
  (= (second head) (second tail)))

(defn move-tail [head tail]
  (if (adjacent? head tail)
    tail
    (let [[xt yt] tail [xh yh] head]
      (cond
        (and
          (not (same-col head tail))
          (not (same-row head tail)))
        ;; move diagonally
        [(if (< xh xt) (dec xt) (inc xt))
         (if (< yh yt) (dec yt) (inc yt))]

        (= 2 (abs (- xh xt)))
        (if (< xh xt)
          [(dec xt) yt]
          [(inc xt) yt])

        (= 2 (abs (- yh yt)))
        (if (< yh yt)
          [xt (dec yt)]
          [xt (inc yt)])))))

(defn do-step [step state]
  (loop [state state
         [dir dis] step]

    (if (< 0 dis)
      (let [new-head (do-move dir (:head state))
            new-tail (move-tail new-head (:tail state))]
        (recur (assoc
                 (update state :positions conj new-tail)
                 :head new-head
                 :tail new-tail)
               [dir (dec dis)]))
      state
      )))


(defn do-part1 [moves]
  (count (set (:positions
                (reduce
                  (fn [state move]
                    (let [result (do-step move state)]
                      result))

                  {:head [0 0] :tail [0 0] :positions []}

                  moves)))))

(deftest test-day9

  (testing "part 1"
    (is (= 6311 (do-part1 (moves input-file)))))

  (testing "test moving horizontally"

    (is (= [2 4] (move-tail [1 4] [3 4])))

    (is (= [2 1] (move-tail [3 1] [1 1])))
    (is (= [-2 1] (move-tail [-3 1] [-1 1]))))

  (testing "test moving vertically"
    (is (= [1 2] (move-tail [1 3] [1 1])))
    (is (= [-1 2] (move-tail [-1 3] [-1 1]))))

  (testing "moving the tail diagonally"

    (is (= [2 2] (move-tail [2 3] [1 1])))
    (is (= [1 2] (move-tail [1 1] [2 3])))
    (is (= [-1 -2] (move-tail [-1 -1] [-2 -3])))
    (is (= [-1 2] (move-tail [-1 1] [-2 3])))))


