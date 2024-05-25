(ns dls.day08
  (:gen-class))

(require '[clojure.edn :as edn])

(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.string :as str])

(def part 1)

(defn parse-input [all-lines]
  (let [lr-ins (first all-lines)
        do-node (fn [result lines first-tag]
                  (if (empty? lines)
                    [result first-tag]
                    (let [m (re-matches #"^(\S+) = [(](\S+), (\S+)[)]" (first lines))
                          node-tag (m 1)
                          next-left (m 2)
                          next-right (m 3)]
                      (recur (assoc result node-tag {:left next-left :right next-right})
                             (rest lines)
                             (if (nil? first-tag) node-tag first-tag)))))
        [nodes first-tag] (do-node {} (rest (rest all-lines)) nil)]
    {:lr-insns (vec lr-ins) :first-tag first-tag :network nodes}))

(defn do-steps [netmap]
;;  (println "do-steps:" netmap)
  (let [do-step (fn [result insns node-tag]
;;                  (println "do-step:" result (take 20 insns) node-tag)
                  (if (= node-tag "ZZZ")
                    result
                    (let [ins (first insns)
                          next (if (= \L ins)
                                 (((netmap :network) node-tag) :left)
                                 (((netmap :network) node-tag) :right))]
;;                      (println "ins:" ins "next:" next)
                      (recur (conj result next) (rest insns) next))))]
    (do-step [] (apply concat (repeat (netmap :lr-insns))) (netmap :first-tag))))

(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(edn/read-string %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])

(defn -main
  "day 8"
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
                  "day08.input"
                  (first arguments))
          steps (->>
                 (slurp fname)
                 (clojure.string/split-lines)
                 (parse-input)
                 (do-steps))]
      (println (count steps)))))
