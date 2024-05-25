(ns dls.day07
  (:gen-class))

(require '[clojure.edn :as edn])

(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.string :as str])

(def part 1)

(defn third [l]
 (first (rest (rest l))))

;; 7 Five of a kind, where all five cards have the same label: AAAAA
;; 6 Four of a kind, where four cards have the same label
;;  and one card has a different label: AA8AA
;; 5 Full house, where three cards have the same label, and
;;  the remaining two cards share a different label: 23332
;; 4 Three of a kind, where three cards have the same label, and
;;  the remaining two cards are each different from any other card in the hand: TTT98
;; 3 Two pair, where two cards share one label, two other cards share a second label,
;;  and the remaining card has a third label: 23432
;; 2 One pair, where two cards share one label, and the other
;;  three cards have a different label from the pair and each other: A23A4
;; 1 High card, where all cards' labels are distinct: 23456
(defn simple-score-hand [hand]
  "calculate absolute score based on hand alone"
  (let [h (sort hand)]
    (cond
      (apply = hand) 7 ; :five-kind
      (or (apply = (take 4 h))
          (apply = (take 4 (rest h)))) 6 ; :four-kind
      (or (and (apply = (take 2 h)) (apply = (take 3 (reverse h))))
          (and (apply = (take 3 h)) (apply = (take 2 (reverse h))))) 5 ; :full-house
      (or (apply = (take 3 h))
          (apply = (take 3 (rest h)))
          (apply = (take 3 (rest (rest h))))) 4 ; :three-kind
      (or (and (apply = (take 2 h)) (apply = (take 2 (rest (rest h)))))                ; AABBX
          (and (apply = (take 2 h)) (apply = (take 2 (rest (rest (rest h))))))         ; AAXBB
          (and (apply = (take 2 (rest h))) (apply = (take 2 (rest (rest (rest h))))))) ; XAABB
      3 ; :two-pair
      (or (apply = (take 2 h))                           ; AAXXX
          (apply = (take 2 (rest h)))                    ; XAAXX
          (apply = (take 2 (rest (rest h))))             ; XXAAX
          (apply = (take 2 (rest (rest (rest h))))))     ; XXXAA
      2 ; :one-pair
      true 1))) ; :high-card

(declare highest-wildcard-hand)
(defn try-all [hand index]
  "Return the highest hand-score if the card at the given index is replaced"
  (let [non-j-cards [\2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A]
        do-all (fn [results cards]
                 (if (empty? cards)
                   results
                   (let [trial-hand (concat (take index hand)
                                           [(first cards)]
                                           (take-last (- 5 index 1) hand))
                         score (if (some #(or (= \j) (= \J) %) trial-hand)
                                 (highest-wildcard-hand trial-hand)
                                 (simple-score-hand trial-hand))]
                     (recur
                      (conj results score)
                      (rest cards)))))]
    (apply max (do-all [] non-j-cards))))

(defn highest-wildcard-hand-m [hand]
  (let [all-scores (fn [results index]  ; return a list of scores with replacing
                                        ; card at index with each non-jack card 
                     (if (>= index 5)
                       results
                       (recur
                        (if (or (= (nth hand index) \j) (= (nth hand index) \J))
                          (conj results (try-all hand index))
                          results)
                        (+ 1 index))))
        wildcard-scores (all-scores [] 0)
        score (if (empty? wildcard-scores)
                (simple-score-hand hand)
                (apply max wildcard-scores))]
    score))

;; Doing this allows us to benefit from being functional.
;; This just remembers previous return values of the function.
(def highest-wildcard-hand (memoize highest-wildcard-hand-m))

(defn card-value [card]
  (cond
    (not (nil? (#{\2 \3 \4 \5 \6 \7 \8 \9} card))) (- (int card) (int \0))
    (or (= card \a) (= card \A)) 14
    (or (= card \k) (= card \K)) 13
    (or (= card \q) (= card \Q)) 12
    (or (= card \j) (= card \J)) (if (= part 1) 11 1)
    (or (= card \t) (= card \T)) 10))
    
(defn parse-hand [hand]
  (map card-value hand))

(defn score-hand [hand]
  (if (= part 1)
    (simple-score-hand hand)
    (highest-wildcard-hand hand)))

(defn break-tie [l r]
  ;; compare card-by-card and return (compare) of first mis-match
  (if (empty? l)
    0
    (let [l0 (first l)
          r0 (first r)]
      (cond
        (< l0 r0) 1
        (> l0 r0) -1
        true (recur (rest l) (rest r))))))

;; Sort in ascending order
(defn compare-hands [l r]
  (let [l-score (score-hand l)
        r-score (score-hand r)]
    (if (= l-score r-score)
      (break-tie (parse-hand l) (parse-hand r))
      (- r-score l-score))))

;; Rank from weakest to strongest
(defn rank-sorted-hands [hands]
  (map-indexed #(conj %2 (+ 1 %1)) (reverse (sort-by first compare-hands hands))))

(defn score-hands [ranked-indexed-hands]
  (map #(* (second %) (third %)) ranked-indexed-hands))

(defn parse-input [all-lines]
  (let [do-hand (fn [result lines]
                  (if (empty? lines)
                    result
                    (let [m (re-matches #"(.....)[ ]+(\d+).*" (first lines))
                          hand (m 1)
                          bid (edn/read-string (m 2))]
                      (recur (conj result [hand bid]) (rest lines)))))]
    (do-hand [] all-lines)))
    
(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(edn/read-string %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])
  
(defn -main
  "day 7"
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
                  "day07.input"
                  (first arguments))
          input (->>
                 (slurp fname)
                 (clojure.string/split-lines)
                 (parse-input)
                 (rank-sorted-hands)
                 (score-hands))]
      (println "Winnings:" (reduce + input)))))
