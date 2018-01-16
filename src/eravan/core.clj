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
  "Counts number of expressions evaluated"
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

(defn create-obj
  "Takes a function-name mapping to create a dynamic lookup"
  [paths]
  (fn
    [path & args]
    (apply (paths path) args)
    )
  )

;the global environment
(defn get_global_env
  []
  (def env
    (atom
     {
      '* *
      'pi 3.14
      })
    )
  (defn get [x] (@env x))
  (defn set [x val]
    (swap!
     env
     (fn [env] (assoc env x val))
     )
    )
  (create-obj
   {
    :get get
    :set set
    }
   )
  )

(def global_env (get_global_env))

(defn evaluate
  "evaluates an expression with environment. Uses global context if no env supplied"
  ([x] (evaluate x global_env))
  ([x env]
   (println x (number? x))
   (cond
     (symbol? x) (env :get x)
     (number? x) x
     
     (= (first x) 'if)
     (cond
       (evaluate (nth x 1) env)
       (nth x 2)

       :else
       (nth x 3)
       )

     (= (first x) 'define)
     (let [key (nth x 1)
           value (evaluate (nth x 2) env)]
       (env :set key value)
       )

     :else
     (let [proc (evaluate (first x) env)
           args (map (fn [arg] (evaluate arg env)) (rest x))
           ]
       (apply proc args)
       )
     )
   )
  )

(defn run
  "parses and evaluates expressions"
  [x]
  (let [exps (parse x)]
    (map evaluate exps)
    )
  )
