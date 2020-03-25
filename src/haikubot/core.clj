(ns haikubot.core
  (:gen-class))

(require '[clojure.string :as str])

(declare dict) ; CMU dict loaded from path given at cmdline.

;; Utils

(defn tokenize [text] (str/split text #" "))

(defn prefixes [coll]
  (map #(take % coll) (range (+ 1 (count coll)))))

;; CMU-Dict

(defn comment? [text]
  "Returns true iff text is a CMU dict comment line"
  (and (not (empty? text))
       (str/starts-with? text ";;;")))

(defn vowel-sound? [phonetic]
  (contains? (set "aeiou") (first (str/lower-case phonetic))))

(defn parse-cmu-dict-line [line]
  (let [tokens (tokenize line)
        atom (str/lower-case (first tokens))
        syllables (count (filter vowel-sound? (rest tokens)))]
    [(keyword atom) syllables]))

(defn parse-cmu-dict [contents]
  (let [lines (str/split contents #"\n")]
    (into (sorted-map) (map parse-cmu-dict-line (remove comment? lines)))))

(defn load-cmu-dict [path]
  (parse-cmu-dict (slurp path)))

;; Errors

(defn unknown-word [word]
  (throw (Exception. (format "unknown word: '%s'" word))))

;; Syllables

(defn count-word-syllables
  "Returns the number of syllables in word."
  [word]
  (let [key  (keyword word)]
    (if (contains? dict key)
        (dict key)
        (unknown-word word))))

(defn count-syllables
  "Returns the total number of syllables in words."
  [words]
  (reduce #(+ %1 (count-word-syllables %2)) 0 words))

(defn has-at-least-n-syllables?
  "True if the input phrase has at least n syllables."
  ([n words]
    (<= n (count-syllables words)))
  ([n]
    (fn [words] (has-at-least-n-syllables? n words))))

(defn take-n-syllables
  "Returns [phrase, rest] if a phrase of exactly n syllables can be read from
   words. rest is all of the words after phrase. Returns [nil, words]
   otherwise."
  [n words]
  (let [phrase (first (filter (has-at-least-n-syllables? n) (prefixes words)))
        rest (drop (count phrase) words)]
      (if (= n (count-syllables phrase))
          [phrase, rest]
          [nil, words])))

;; Haiku

(defn haiku-ijk [i j k words]
  "Returns a haiku from words with lines of i j and k syllables respectively.
   Returns nil words is not a haiku with the given syllables."
  (let [[l1 rest] (take-n-syllables i words)
        [l2 rest] (take-n-syllables j rest)
        [l3 rest] (take-n-syllables k rest)]
    (if (and (empty? rest) (not-any? nil? [l1 l2 l3]))
      [l1, l2, l3]
      nil)))

(defn print-haiku [lines]
  (do
    (println (first lines))
    (println (second lines))
    (println (nth lines 2))))

;; Main program

(defn usage [] (println "haikubot [cmu-dict-path]"))

(defn run []
  (let [input  (read-line)
        words  (tokenize input)
        haiku575 (haiku-ijk 5 7 5 words)
        haiku566 (haiku-ijk 5 6 6 words)
        haiku654 (haiku-ijk 6 5 6 words)
        haiku664 (haiku-ijk 6 6 4 words)]
      (cond
        (not (nil? haiku575)) (print-haiku haiku575)
        (not (nil? haiku566)) (print-haiku haiku566)
        (not (nil? haiku654)) (print-haiku haiku654)
        (not (nil? haiku664)) (print-haiku haiku664)
        :else (println (format "not a haiku: '%s'\n" input)))))

(defn -main [& args]
  (if (empty? args)
      (usage)
      (do
        (def dict (load-cmu-dict (first args)))
        (run))))

