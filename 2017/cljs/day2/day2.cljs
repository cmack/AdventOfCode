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

(defn- line-checksum [line]
  (- (apply max line)
     (apply min line)))

(defn checksum [input]
  (reduce + (map line-checksum (listify-input input))))

(print (checksum input))
