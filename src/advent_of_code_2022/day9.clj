(ns advent-of-code-2022.day9
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def sample-file "resources/day9-sample.txt")
(def larger-sample-file "resources/day9-larger.txt")
(def input-file "resources/day9.txt")

(defn moves [file-path]
  (map
    (fn [l]
      (let [[dir dis] (clojure.string/split l #" ")]
        [dir (parse-int dis)]))
    (parse-lines file-path)))

(defn single-moves [file-path]
  (apply concat
         (map
           (fn [[dir dis]] (repeat dis dir))
           (moves file-path))))

(defn do-move [pos dir]
  (let [[x y] pos]
    (condp = (str dir)
      "R" [(inc x) y]
      "L" [(dec x) y]
      "U" [x (inc y)]
      "D" [x (dec y)])))

(defn calculate-trail [mvs]
  (let [start [0 0]]
    (reduce
      (fn [state mv]
        (let [new-pos (do-move (last state) mv)]
          (conj state new-pos)))
      [start]
      mvs)))

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
      (let [new-head (do-move (:head state) dir)
            new-tail (move-tail new-head (:tail state))]
        (recur (assoc
                 (update state :positions conj new-tail)
                 :head new-head
                 :tail new-tail)
               [dir (dec dis)]))
      state
      )))

(defn get-head-positions [moves]
  (let [do-step (fn [trail step]
                  (loop [trail trail
                         [dir dis] step]
                    (if (< 0 dis)
                      (let [new-head (do-move (peek trail) dir)]
                        (recur (conj trail new-head)
                               [dir (dec dis)]))
                      trail)))]
    (reduce do-step [[0 0]] moves)))

(defn get-tail-positions [moves]
  (:positions
    (reduce

      (fn [state move]
        (let [result (do-step move state)]
          result))

      {:head [0 0] :tail [0 0] :positions []}

      moves)))

(defn do-part1 [moves]
  (count (set (get-tail-positions moves))))

(defn do-part1-refactor [file-path]
  (count (set (reduce
                (fn [agg head-pos]
                  (conj agg (move-tail head-pos (last agg))))
                [[0 0]]
                (calculate-trail (single-moves file-path)))))
  )

(let [heads (get-head-positions (moves larger-sample-file))
      move-knots (fn [knot] (reduce
                              (fn [agg head]
                                (conj agg (move-tail head (peek agg))))
                              [[0 0]]
                              knot))]

  (count (set (nth (iterate move-knots heads) 9))))


(defn process-knots [file-path no-knots]
  (let [heads (get-head-positions (moves file-path))
        move-knots (fn [knot] (reduce
                                (fn [agg head]
                                  (conj agg (move-tail head (peek agg))))
                                [[0 0]]
                                knot))]

    (count (set (nth (iterate move-knots heads) no-knots)))))

(deftest test-day9

  (testing "part 2"
    (is (= 36 (process-knots larger-sample-file 9)))
    (is (= 2482 (process-knots input-file 9))))

  (testing "part 1"
    (is (= 6311 (do-part1 (moves input-file)))))

  (testing "part 1 refactor"

    (is (= 13 (do-part1-refactor sample-file)))
    (is (= 6311 (do-part1-refactor input-file))))

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


