(ns dls.day03
  (:gen-class))

(require '[clojure.tools.cli :refer [parse-opts]])

(def part 1)
(def schematic [])

;; (defn maybe-integer [s]
;;   "Return the integer for the given string, or false"
;;   (try (Integer/parseInt s)
;;                (catch NumberFormatException e false)))

(defn number-coords [row col]
  "Return a vec of the coordinates of all the digits starting at the given position"
  (defn number-coords-1 [result r c]
    (if (>= c (alength (aget schematic r)))
      result
      (if (Character/isDigit (aget schematic r c))
        (recur (conj result [r c]) r (+ 1 c))
        result)))
  (number-coords-1 [] row col))

(defn symbol-char-at-pos? [pos]
  (try (let [[row col] pos]
         (let [c (aget schematic row col)]
           (not (or (= c \.) (Character/isDigit c)))))
       (catch ArrayIndexOutOfBoundsException e false)))

(defn positions-surrounding-area [row col area]
  "Given the position of a part number, return the positions surrounding it"
  (let [deltas [[-1 -1] [-1 0] [-1 1]
                [0 -1] [0 1]
                [1 -1] [1 0] [1 1]]
        num-coords (number-coords row col)]
    (if (empty? num-coords)
      nil
      (set (for [[p-r p-c] num-coords
                 [d-r d-c] deltas]
             [(+ p-r d-r) (+ p-c d-c)])))))

(defn positions-surrounding-part-number [row col]
  "vector of positions surrounding vector of positions of a part number"
  (positions-surrounding-area row col (number-coords row col)))

(defn positions-surrounding [row col]
  "vector of positions surrounding one given position"
  (positions-surrounding-area row col [row col]))

(defn part-number-at [row col]
  "if there is a number at the given position, return [len-of-number number], or nil"
  (let* [num-coords (number-coords row col)
         len (count num-coords)]
    [len (Integer/parseInt (apply str (subvec (vec (aget schematic row)) col (+ col len))))]))

(defn part-record-at-pos [row col]
  "if there is a number at the given position, return a map with len, pos, number, and surrounding positions"
  (let [coords (positions-surrounding-part-number row col)]
    (if (every? #(= false %) (map symbol-char-at-pos? coords))
      nil
      (let [[len num] (part-number-at row col)]
        {:len len
         :part-number num
         :position [row col]
         :surrounding-coords coords}))))

(defn schematic-pos-next [row col delta]
  "return the next pos or nil"
  (let [width (count (aget schematic 0))
        height (count schematic)]
    (if (>= row height)
      nil
      (let [c (+ col delta)]
        (if (< c width)
          [row c]
          (let [r (+ row 1)]
            (if (>= r height)
              nil
              [r 0])))))))

(defn gear-ratio-at-pos [partrecs row col]
  "if there are exactly 2 part-numbers adjacent to [row col], return their product, else 0"
  (if (not= \* (aget schematic row col))
    0
    ;; traverse partnums, find ones that have a surrounding position
    ;; equal to the "*" position.  If that list is length 2, return ratio.
    (let [neighbors (for [partrec partrecs
                          :when (> (count (filter #(= [row col] %) (:surrounding-coords partrec))) 0)]
                      (:part-number partrec))]
      (if (= 2 (count neighbors))
        (reduce * neighbors)
        0))))

(defn parse-part-numbers []
  "Return a vector of the part numbers in schematic"
  (defn parse-part-numbers-1 [result row col]
    (if (= nil row)
      result
      (let [part-record (part-record-at-pos row col)]
        (let [{partnum :part-number len :len} part-record]     ; is there a partnum here?
          (let [nextpos (if partnum len 1)]                    ; if so skip over it, else next column
            (let [[r c] (schematic-pos-next row col nextpos)]  ; get the next [r c] (or nil)
              (recur (if partnum (conj result part-record) result) r c))))))) ; move on to the next pos
  (parse-part-numbers-1 [] 0 0))

(defn parse-gear-ratios [partrecs]
  "Return a vector of the gear ratios in schematic"
  (defn parse-gear-ratios-1 [result partrecs row col]
    (if (= nil row)
      result
      (let [gear-ratio (gear-ratio-at-pos partrecs row col)  ; is there a gear-ratio here?
            [r c] (schematic-pos-next row col 1)]            ; get the next [r c] (or nil)
        (recur (conj result gear-ratio) partrecs r c))))     ; move on to the next pos
  (parse-gear-ratios-1 [] partrecs 0 0))

(def cli-options [["-h" "--help" "Print this help" :default false]
                  ["-p" "--part 1|2" "The part of puzzle to solve."
                   :default 1
                   :parse-fn #(Integer/parseInt %)
                   :validate [#(< 0 % 3) "Must be a number between 0 and 3"]]])

(defn -main
  "day 3"
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (when (:help options)
      (println summary)
      (System/exit 1))
    (when errors
      (println (first errors)))

    (def part (:part options))
    (let [fname (if (empty? arguments)
                  "day03.input"
                  (first arguments))]
      ;; This is just laziness -- didn't feel like passing it around
      (def schematic (to-array-2d (clojure.string/split-lines (slurp fname))))
      (let [part-numbers (parse-part-numbers)]
        (println (reduce + (if (= 1 part)
                             (map #(:part-number %) part-numbers)
                             (parse-gear-ratios part-numbers))))))))
