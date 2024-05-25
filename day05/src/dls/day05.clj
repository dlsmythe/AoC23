(ns dls.day05
  (:gen-class))

(require '[clojure.edn :as edn])

(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.string :refer [trim]])

(def part 1)

(defn third [l]
 (first (rest (rest l))))

(defn src-to-dst [rmap src-number]
  "This maps the src->dest number based on the ranges in the given map"
  (loop [r (:ranges rmap)]
    (if (= 0 (count r))
      src-number         ; if not in a range, dst=src
      (let [dst-lo (nth (first r) 0)
            src-lo (nth (first r) 1)
            r-len (nth (first r) 2)
            translated-src (+ dst-lo (- src-number src-lo))]
        (if (<= dst-lo translated-src (+ dst-lo r-len -1))
          translated-src
          (recur (rest r)))))))

(defn prng [rng]
  (if (and rng (second rng))
    (if (first rng)
      (format "[%d-%d -> %d-%d] " (second rng) (+ (second rng) (third rng) -1) (first rng) (+ (first rng) (third rng) -1))
      (format "[%d, %d] " (second rng) (+ (second rng) (third rng) -1)))
    "(nil)"))

;; Splitting source ranges into parts that overlap dest ranges.

;; This presumes that dst-ranges is sorted by dst-low (nth rng 1)
(defn find-dst-range [dst-ranges src-low]
  "Find the dst-range that overlaps or contains the src-low position"
;;  (println "dst-ranges:" dst-ranges "src-low:" src-low)
  (if (not= 0 (count dst-ranges))
    (let [range (first dst-ranges)
          [out-low in-low rng-len] range]
      (if (<= src-low (+ in-low rng-len -1))
        range ; found it!
        (recur (rest dst-ranges) src-low)))
    [nil nil nil]))

;; NB: These ranges are vecs of (dst-low src-low len), which is exactly what is in the range-map.
(defn translate-ranges [arg-src-ranges arg-dst-ranges]
  "Apply destination range maps to src ranges, splitting src ranges as required."
  (println "src-rngs:" arg-src-ranges "dst-rngs:" arg-dst-ranges)
  (let [dst-ranges (sort-by second arg-dst-ranges) ; we want the to-be-mapped ranges in order
        handle-ranges
        (fn [results src-ranges src-low src-len]
          ;; This passes in both the list and the current low/len to support splitting a src-rng
          (println "handle-ranges: results:" results "src-ranges:" src-ranges "cur-src: " (prng [nil src-low src-len]))
          (if (not src-low)
            (do
              (println "XLATED:" results)
              results)
            (let [[out-low in-low rng-len] (find-dst-range dst-ranges src-low)]
;;              (println "range match:" (prng [out-low in-low rng-len]))
              (if out-low  ; then src-low overlaps some dst-range
                (if (< src-low in-low) ; handle src overlap on left
                  (let [new-len (- in-low src-low)]
                    (println "src overlap on left")
                    (recur (conj results [nil in-low new-len])
                           src-ranges
                           (+ src-low new-len)
                           (- src-len new-len)))
                  ;; src-low starts at or within some dst-rng
                  (let [src-end (+ src-low src-len -1)
                        in-end (+ in-low rng-len -1)]
                    (println "src" (prng [nil src-low src-len]) "is at or within dst:" (prng [out-low in-low rng-len]) )
                    (if (<= src-len rng-len) ; src-rng is within dst-rng
                      (let [next-src-range (first (rest src-ranges))]
                        (println "src is contained in dst")
                        (recur (conj results [nil out-low rng-len])
                               (rest src-ranges)
                               (if next-src-range (second next-src-range) nil)  ; src-low
                               (if next-src-range (third next-src-range) nil))) ; src-len
                      ;; src-rng hangs over dst-rng -- split
                      (let [hangover-len (- src-end in-end)
                            new-len (- src-len hangover-len)]
                        (println "src:" src-low "," src-end "dst:" in-low "," in-end)
                        (println "src hangs over " hangover-len "on the right")
                        (println "adding" out-low "," new-len)
                        (recur (conj results [nil out-low new-len])
                               src-ranges
                               (+ src-low new-len)
                               hangover-len)))))

                ;; else src-rng is not in any dst-rng
                (do
                  (println "src rng not overlapping any dst rng")
                  (recur (conj results [nil src-low src-len])
                         (rest src-ranges)
                         (if (rest src-ranges) (second (first (rest src-ranges))) nil)  ; src-low
                         (if (rest src-ranges) (third (first (rest src-ranges))) nil)))))))] ; src-len
    (handle-ranges #{}
                   arg-src-ranges
                   (second (first arg-src-ranges))  ; src-low
                   (third (first arg-src-ranges))))) ; src-len


(defn foo-seeds [seed-numbers]
  "Just a hack to print out the number of seeds to be processed"
  (let [num-seeds (reduce + (for [[_ lo len] (partition 3 seed-numbers)] len))]
    (println "#seeds: " num-seeds)))

(defn parse-input [lines]
  (let [seed-numbers (map #(edn/read-string %) (re-seq #"\d+" (subs (first lines) (count "seeds:"))))
        handle-section
        (fn [range-maps src-name dst-name dst-ranges sec-lines]
          (let [line (first sec-lines)
                secname-match (if line (re-matches #"(\S+)-to-(\S+) map:" line) nil)
                range-match (if line (re-matches #"(\d+) (\d+) (\d+)" line) nil)]
            ;; (println "line: '" line "'")
            ;; (println "sm: " secname-match)
            ;; (println "rm: " range-match)
            (cond
              ;; handle gap between sections
              (= 0 (count (trim (if line line ""))))
              (let [range-maps (conj range-maps
                                     {:dst dst-name
                                      :src src-name
                                      :ranges dst-ranges})]
                ;; Is this the last new map section?
                (if (= 0 (count (rest sec-lines)))
                  (do (foo-seeds seed-numbers) [seed-numbers range-maps])
                  (recur range-maps "" "" [] (rest sec-lines))))

              ;; Parse the section name line
              secname-match (recur
                             range-maps
                             (nth secname-match 1) ; src
                             (nth secname-match 2) ; dst
                             [] (rest sec-lines))
              
              ;; Parse a range within a map section
              range-match (recur
                           range-maps
                           src-name
                           dst-name
                           (conj dst-ranges [(edn/read-string (nth range-match 1))
                                             (edn/read-string (nth range-match 2))
                                             (edn/read-string (nth range-match 3))])
                           (rest sec-lines))

              true (do
                     (println "Syntax error: " line)
                     (System/exit 1)))))]
    (handle-section [] "" "" [] (rest (rest lines)))))

(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(edn/read-string %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])
  
(defn -main
  "day 5"
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (when (:help options)
      (println summary)
      (System/exit 1))
    (when errors
      (println (first errors))
      (System/exit 1))

    (def part (:part options))
    (println "Reading input...")
    (let [fname (if (empty? arguments)
                  "day05.input"
                  (first arguments))
          [seed-line-seeds range-maps] (->>
                                        (slurp fname)
                                        (clojure.string/split-lines)
                                        (parse-input))
          seeds (if (= part 1)
                  seed-line-seeds
                  ;; Part 2 must put the seeds in the format of the range-map :ranges
                  (apply concat (for [[lo len] (partition 2 seed-line-seeds)] [nil lo len])))]

      (println "seed-line-seeds: " seed-line-seeds)
      (println (count seeds) "seeds")
      (println "seeds: " seeds)
      ;; (println "range-maps: " range-maps)

      (when (= part 1)
        (let [soils-for-seeds (map (fn [src-num] (src-to-dst (first (filter #(= "soil" (:dst %)) range-maps)) src-num)) seeds)
              fertilizer-for-soils (map (fn [src-num]
                                          (src-to-dst
                                           (first (filter #(= "fertilizer" (:dst %)) range-maps))
                                           src-num)) soils-for-seeds)
              water-for-fertilizer (map (fn [src-num]
                                          (src-to-dst
                                           (first (filter #(= "water" (:dst %)) range-maps))
                                           src-num)) fertilizer-for-soils)
              light-for-water (map (fn [src-num]
                                     (src-to-dst
                                      (first (filter #(= "light" (:dst %)) range-maps))
                                      src-num)) water-for-fertilizer)
              temperature-for-light (map (fn [src-num]
                                           (src-to-dst
                                            (first (filter #(= "temperature" (:dst %)) range-maps))
                                            src-num)) light-for-water)
              humidity-for-temperature (map (fn [src-num]
                                              (src-to-dst
                                               (first (filter #(= "humidity" (:dst %)) range-maps))
                                               src-num)) temperature-for-light)
              location-for-humidity (map (fn [src-num]
                                           (src-to-dst
                                            (first (filter #(= "location" (:dst %)) range-maps))
                                            src-num)) humidity-for-temperature)]

          ;; (println "soils-for-seeds: " soils-for-seeds)
          ;; (println "fertilizer-for-soils: " fertilizer-for-soils)
          ;; (println "water-for-fertilizer: " water-for-fertilizer)
          ;; (println "light-for-water: " light-for-water)
          ;; (println "temperature-for-light: " temperature-for-light)
          ;; (println "humidity-for-temperature: " humidity-for-temperature)
          ;; (println "location-for-humidity: " location-for-humidity)

          (let [nearest (first (sort location-for-humidity))
                corresponding-seed (nth seeds (.indexOf location-for-humidity nearest))]
            (println "nearest location is" nearest "which is for seed" corresponding-seed))))

      (let [seed-ranges (partition 3 seeds)
            soil-ranges (:ranges (first (filter #(= "soil" (:dst %)) range-maps)))
            fertilizer-ranges (:ranges (first (filter #(= "fertilizer" (:dst %)) range-maps)))
            water-ranges (:ranges (first (filter #(= "water" (:dst %)) range-maps)))
            light-ranges (:ranges (first (filter #(= "light" (:dst %)) range-maps)))
            temperature-ranges (:ranges (first (filter #(= "temperature" (:dst %)) range-maps)))
            humidity-ranges (:ranges (first (filter #(= "humidity" (:dst %)) range-maps)))
            location-ranges (:ranges (first (filter #(= "location" (:dst %)) range-maps)))]

        (println "seed-ranges" seed-ranges)
        (println "soil-ranges" soil-ranges)
        (println "fertilizer-ranges" fertilizer-ranges)
        (println "water-ranges" water-ranges)
        (println "light-ranges" light-ranges)
        (println "temperature-ranges" temperature-ranges)
        (println "humidity-ranges" humidity-ranges)
        (println "location-ranges" location-ranges)
        
        (let [seed-to-soil-ranges (translate-ranges seed-ranges soil-ranges)
              soil-to-fertilizer-ranges (translate-ranges seed-to-soil-ranges fertilizer-ranges)
              fertilizer-to-water-ranges (translate-ranges soil-to-fertilizer-ranges water-ranges)
              water-to-light-ranges (translate-ranges fertilizer-to-water-ranges light-ranges)
              light-to-temperature-ranges (translate-ranges water-to-light-ranges temperature-ranges)
              temperature-to-humidity-ranges (translate-ranges light-to-temperature-ranges humidity-ranges)
              humidity-to-location-ranges (translate-ranges temperature-to-humidity-ranges location-ranges)]

          (println "seed -> soil rngs:" seed-to-soil-ranges)
          (println "seed -> soil rngs:" (map prng seed-to-soil-ranges))
          (println "seed -> soil -> fertilizer rngs:" soil-to-fertilizer-ranges)
          (println "seed -> soil -> fertilizer -> water rngs:" fertilizer-to-water-ranges)
          (println "seed -> soil -> fertilizer -> water -> light rngs:" water-to-light-ranges)
          (println "seed -> soil -> fertilizer -> water -> light -> temperature rngs:" light-to-temperature-ranges)
          (println "seed -> soil -> fertilizer -> water -> light -> temperature -> humidity rngs:" temperature-to-humidity-ranges)

          (let [seed-location-ranges (sort-by first humidity-to-location-ranges)]
            (println "seed -> location ranges:" seed-location-ranges))))

          )))
