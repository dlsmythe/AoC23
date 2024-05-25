(ns dls.day04
  (:gen-class))

(require '[clojure.tools.cli :refer [parse-opts]])
(use '[clojure.set])
(use '[clojure.math :only [pow]])

(def part 1)

(defn parse-card [card-parts]
  "Turn the parsed line of text into a map representing a card"
  (let [card-number (Integer/parseInt (first card-parts))
        winning-numbers (map #(Integer/parseInt %) (re-seq #"\d+" (nth card-parts 1)))
        my-numbers (map #(Integer/parseInt %) (re-seq #"\d+" (nth card-parts 3)))]
    (let [my-winning-numbers (intersection (set winning-numbers) (set my-numbers))]
      {:card-number card-number
       :winning-numbers winning-numbers
       :my-numbers my-numbers
       :my-winning-numbers my-winning-numbers})))

(defn card-value [card]
  "Score this card, using the rules from part 1"
  (if (empty? (:my-winning-numbers card))
    0
    (int (pow 2 (- (count (:my-winning-numbers card)) 1)))))

(defn card-with-number [num cards]
  (first (filter #(= (:card-number %) num) cards)))

(defn dump-card [card]
  (println (format "Card %d: winners: " (:card-number card)) (:my-winning-numbers card)))

(defn dump-cards [msg cards]
  (println (format "%s: " msg) "======== " (count cards) " Cards ===========")
;  (println "cards: " cards)
  (loop [remaining cards]
    (when (> (count remaining) 0)
      (dump-card (first remaining))
      (recur (rest remaining)))))

(defn gather-new-cards [from-card deck]
  (loop [remaining (count (:my-winning-numbers from-card))
         next-card-number (+ (:card-number from-card) 1)
         results []]
    (if (= 0 remaining)
      results
      (recur (- remaining 1)
             (+ next-card-number 1)
             (conj results (card-with-number next-card-number deck))))))

(defn process-cards-part2 [all-cards]
  (println "Processing" (count all-cards) "cards")
  (loop [remaining-cards (seq all-cards)
         results []]
    (when (= 0 (rem (count remaining-cards) 500))
      (println "Total remaining:" (count remaining-cards) "current:" (:card-number (first remaining-cards))))
    (if (= 0 (count remaining-cards))
      results
      (let [new-cards (gather-new-cards (first remaining-cards) all-cards)]
        (recur (if (> (count new-cards) 0)
                 (concat (rest remaining-cards) new-cards)
                 (rest remaining-cards))
               (conj results (first remaining-cards)))))))

(defn parse-cards [lines]
  "Return a vector of parsed cards"
  (loop [cards lines
         results []]
    (if (or (= 0 (count cards)) (= 0 (count (first cards))))
      results
      (let [match (re-matches #"^Card[ ]+(\d+):(([ ]+\d+)+) [|](([ ]+\d+)*)" (first cards))]
        (recur (rest cards) (conj results (parse-card (rest match))))))))

(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(Integer/parseInt %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])

(defn -main
  "day 4"
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
                  "day04.input"
                  (first arguments))
          cards (->>
                 (slurp fname)
                 (clojure.string/split-lines)
                 (parse-cards))]
 ;     (println "cards: " cards)
      (if (= part 1)
        (println (reduce + (map card-value cards)))
        (println (count (process-cards-part2 cards)))))))
