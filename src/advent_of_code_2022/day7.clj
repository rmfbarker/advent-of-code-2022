(ns advent-of-code-2022.day7
  (:require [clojure.test :refer :all]
            [advent-of-code-2022.core :refer :all]))

(def day7-sample "resources/day7-sample.txt")
(def day7-input "resources/day7.txt")

(def cmds (parse-lines day7-input))

(defn file-size [l]
  (if-let [sz (first (re-seq
                       #"\d+"
                       l))]
    (parse-int sz)
    ))

(defn process [file-path]
  (let [cmds (parse-lines file-path)]
    (reduce (fn [{:keys [current-dir] :as agg} c]
              (if (clojure.string/starts-with? c "$ cd")
                (do (println "change dir" c)
                    (let [new-dir (last (clojure.string/split c #" "))
                          new-agg (if (= ".." new-dir)
                                    (pop current-dir)
                                    (conj current-dir new-dir))]
                      (println "moving to dir" new-dir)
                      (println "new agg" new-agg)
                      (assoc agg :current-dir new-agg)))
                agg))
            {:current-dir []}
            cmds)))

(defn all-parent-dirs [current-dir]
  (loop [current-dir current-dir
         dirs        []]
    (if (seq current-dir)
      (recur
        (pop current-dir)
        (conj dirs current-dir))
      dirs)))

(defn parse-files [file]
  (loop [cmds        (parse-lines file)
         current-dir []
         files       []]
    (if (seq cmds)
      (let [c (first cmds)]
        (cond
          (clojure.string/starts-with? c "dir")
          (do (println "in a dir" c "parent dir is" current-dir)
              (recur (rest cmds) current-dir files))

          (file-size c)
          (let [parent-dirs  (all-parent-dirs current-dir)
                dirs-with-sz (map vector parent-dirs (repeat (file-size c)))]
            (recur (rest cmds) current-dir (into files dirs-with-sz)))

          (clojure.string/starts-with? c "$ cd")
          (do (println "change dir" c)
              (let [new-dir (last (clojure.string/split c #" "))
                    new-agg (if (= ".." new-dir)
                              (pop current-dir)
                              (conj current-dir new-dir))]
                (println "moving to dir" new-dir)
                (println "new agg" new-agg)
                (recur (rest cmds) new-agg files)))

          :else
          (recur (rest cmds) current-dir files)))
      files)))

(defn dir-sizes [file] (map
                         (fn [[dir files]]
                           [dir (reduce + (map second files))]
                           )

                         (group-by first (parse-files file))))

(defn calculate [file]
  (transduce (comp (filter
                     (fn [[dir sz]] (< sz 100000)))
                   (map second))
             +
             (dir-sizes file)))