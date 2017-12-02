;;; http://adventofcode.com/2017/day/2

(ns cmack.adventofcode
  (:require [lumo.io :as io]
            [clojure.string :as string]))

(def input (io/slurp "../../inputs/day2/day2.txt"))

(defn- listify-input [input-string]
   (-> input-string
         (string/split "\n")
         (->> (map #(string/split % "\t"))
              (map (partial map int)))))

(defn- max-min-distance [line]
  (- (apply max line)
     (apply min line)))

(defn- divisible-combinations [sequence]
  (for [x sequence
        y sequence
        :when (and (not= x y)
                   (> x y)
                   (zero? (rem x y)))]
    (list x y)))

(defn- evenly-divisible [line]
  (->> (divisible-combinations line)
       (map (partial apply /))
       (first)))

(defn checksum [input line-checksum-fn]
  (reduce + (map line-checksum-fn (listify-input input))))

(println (checksum input max-min-distance))
(println (checksum input evenly-divisible))
