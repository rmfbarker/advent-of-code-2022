(ns advent-of-code-2022.core
  (:gen-class))

(defn parse-int [s] (Integer/parseInt (str s)))
(defn parse-lines [input-file] (clojure.string/split-lines (slurp input-file)))
