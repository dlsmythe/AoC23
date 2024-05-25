(ns dls.day06
  (:gen-class))

(require '[clojure.edn :as edn])

(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.string :as str])

(def part 1)

(defn find-winners [arg]
  (let [[last-time record-dist] arg
        do-one-timeval (fn [results cur-time]
                         (if (>= cur-time last-time)
                           results
                           (let [duration (- last-time cur-time)
                                 race-dist (+ (* duration cur-time))]
                             (recur
                              (if (> race-dist record-dist)
                                (conj results race-dist)
                                results)
                              (+ cur-time 1)))))]
    (do-one-timeval [] 1)))

(defn winners-per-race [history]
  (let [do-race (fn [results hist]
                  (if (empty? hist)
                    results
                    (recur (conj results (find-winners (first hist)))
                           (rest hist))))]
    (map count (do-race [] history))))

(defn parse-input [lines]
  (let [times-str-list (re-seq #"\d+" (subs (first lines) (count "Time:")))
        dists-str-list (re-seq #"\d+" (subs (second lines) (count "Distance:")))]
    (if (= part 1)
      (partition 2 (interleave (map #(edn/read-string %) times-str-list)
                               (map #(edn/read-string %) dists-str-list)))
      [[(edn/read-string (clojure.string/join "" times-str-list))
       (edn/read-string (clojure.string/join "" dists-str-list))]])))

                                                                           
(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(edn/read-string %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])
  
(defn -main
  "day 6"
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (when (:help options)
      (println summary)
      (System/exit 1))
    (when errors
      (println (first errors))
      (System/exit 1))

    (def part (:part options))
    (let [fname (if (empty? arguments)
                  "day06.input"
                  (first arguments))
          race-history (->>
                        (slurp fname)
                        (clojure.string/split-lines)
                        (parse-input)
                        (winners-per-race))]
      (println "Score:" (reduce * race-history)))))
