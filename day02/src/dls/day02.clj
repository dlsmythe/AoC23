(ns dls.day02
  (:gen-class))

(require '[clojure.tools.cli :refer [parse-opts]])

(def part 1)
(def limits {:red 12 :green 13 :blue 14})

(defn parse-draw-color [s]
  (let [m (re-matches #"[ ,]*(\d+) (red|green|blue).*" s)]
    {(keyword (m 2)) (Integer/parseInt (m 1))}))

                                        ; returns a dict
(defn parse-draw [d]
  (apply merge (map parse-draw-color (re-seq #"[^,]+" d))))

(defn parse-game [line]
  (let [m (re-matches #"Game (\d+): (.*)$" line)]
    {:game (Integer/parseInt (m 1))
     :draws (map parse-draw (re-seq #"[^;]+" (m 2)))}))

(defn possible-draw [draw]
  (apply = true (map #(<= (draw %) (limits %)) (keys draw))))

(defn score [game]
  (let [s (if (apply = true (map possible-draw (game :draws))) (game :game) 0)]
    (if (= part 1)
      s
      (apply * (map (fn [color] (apply max (map #(let [n (color %)] (if n n 0)) (game :draws)))) (keys limits))))))

(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(Integer/parseInt %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])

(defn -main
  "day 2"
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    ;; (println "options " options)
    ;; (println "arguments " arguments)
    ;; (println "summary " summary)
    ;; (println "errors " errors)
    (when (:help options)
      (println summary))
    (when errors
      (println (first errors)))

    (def part (:part options))
    (let [fname (if (empty? arguments)
                  "day02.input"
                  (first arguments))]
      (println
       (reduce + 
               (->>
                (slurp fname)
                (clojure.string/split-lines)
                (map parse-game)
                (map score)))))))
