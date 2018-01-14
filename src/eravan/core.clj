(ns eravan.core)

(require '[clojure.string :as string])
(def replace string/replace)
(def split string/split)

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn tokenize
  "Convert a string of characters into a list of tokens."
  [str]
  (-> str
      (replace "(" " ( ")
      (replace ")" " ) ")
      (split #"\s")
      (#(filter (complement empty?) %1))
      )
  )

(defn atomise
  "Tries to convert a unit to a basic type"
  [x]
  (try
    (parse-int x)
    (catch Exception ex
      (try
        (Float/parseFloat x)
        (catch Exception ex2
          (symbol x)
          )
        )
      )
    )
  )

(defn count-deep
  "This needs to be written keeping in mind number of items skipped"
  [items]
  (cond
    (sequential? items) (reduce + 2 (map count-deep items))
    :else 1
    )
  )

(defn read-from-tokens-stream
  "Read an expression from a sequence of tokens"
  [tokens]
  (cond
    (empty? tokens)
    nil
    
    (= (first tokens) "(")
    (let
        [values (read-from-tokens-stream (rest tokens))
         n (count-deep values)]
      (cons
       values
       (read-from-tokens-stream
        (drop n tokens))
       )
      )

    (= (first tokens) ")") nil

    :else
    (cons
     (atomise (first tokens))
     (read-from-tokens-stream (rest tokens))
     )
    
    )
  )

(defn parse
  "Tokenizes and then parses the input"
  [str]
  (-> str
      (tokenize)
      (read-from-tokens-stream)
      )
  )
