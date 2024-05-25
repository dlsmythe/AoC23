(ns dls.day01
  (:gen-class))

(require '[clojure.edn :as edn])
(require '[clojure.tools.cli :refer [parse-opts]])

(def part 1)

(defn line-to-num-1 [line]
  (Integer/parseInt (str (re-find #"\d" line)
                         (re-find #"\d" (str (reverse line))))))

(defn parse-line [line]
  (defn parse-1 [result line]
    (do
      (if (empty? line)
          result
          (let [m (re-matches #"^(\d|one|two|three|four|five|six|seven|eight|nine|zero).*$" (apply str line))]
            (recur (if m
                     (conj result (case (m 1)
                                    ("1" "one") "1"
                                    ("2" "two") "2"
                                    ("3" "three") "3"
                                    ("4" "four") "4"
                                    ("5" "five") "5"
                                    ("6" "six") "6"
                                    ("7" "seven") "7"
                                    ("8" "eight") "8"
                                    ("9" "nine") "9"
                                    ("0" "zero") "0"))
                     result)
                   (rest line))))))
  (parse-1 [] line))

(defn line-to-num-2 [line]
  (let* [digits (parse-line line)
         n (Integer/parseInt (str (first digits) (first (reverse digits))))]
    n))

    
(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(edn/read-string %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])
  
(defn -main
  "AoC day 1"
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
                  "day01.input"
                  (first arguments))]
      (println
       (reduce + 
               (->>
                (slurp fname)
                (clojure.string/split-lines)
                (map (if (= 1 part) line-to-num-1 line-to-num-2))
                (into [])))))))

