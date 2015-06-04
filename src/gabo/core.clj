(ns gabo.core)

(defmacro when-match
  "Syntax:
  (when-match string
    [sym1 regex1] form1
     ...
    [symN regexN] formN)

  Usage: matches string to every regex, if there is a match then
  the form will be evaluated and returned.
  "
  [string & forms]
  (cons `cond
        (loop [form-pairs (partition 2 forms)
               result []]
          (if (empty? form-pairs)
            result
            (let [[[sym regex] body] (first form-pairs)]
              (recur (rest form-pairs)
                     (conj result
                           `(re-find ~regex ~string)
                           `(let [~sym (re-find ~regex ~string)]
                              ~body))))))))

(defn tokenize-chunk
  [code]
  (when-match code
    [new-lines #"\A\s+"] [:literal new-lines]
    [sym #"\A\{\{\w+\}\}"] [:symbol sym]
    [iter-init #"\A\{\{#\s*\w+\s*\S+?\s*\}\}"] [:iter-init iter-init]
    [iter-end #"\A\{\{/\w+\}\}"] [:iter-end iter-end]
    [literal #"\A.+?(?=\{\{)"] [:literal literal]
    [literal #".*"] [:literal literal]))

(defn tokenize
  "Takes as argument a string and returns a vector
  of [:token-id token-match]"
  [string]
  (loop [tokens []
         code string]
    (if (empty? code)
      tokens
      (let [[token match] (tokenize-chunk code)]
        (recur
          (conj tokens [token match])
          (.substring code (count match)))))))

(defn is-literal
  "Returns true if the given token is a :literal,
  false otherwise."
  [token]
  (= (first token) :literal))

(defn is-symbol
  "Returns true if the given token is a :symbol,
  false otherwise."
  [token]
  (= (first token) :symbol))

(defn is-iter-init
  [token]
  (= (first token) :iter-init))

(defn is-iter-end
  [token]
  (= (first token) :iter-end))

(defn merge-literals
  "merges two literal tokens into one"
  [token1 token2]
  (vector :literal
          (str (last token1)
               (last token2))))

(defn join-literals
  "Given a collection of tokens, this function
  will return a collection of tokens where all
  :literal consequent tokens will be merged"
  [tokens]
  (loop [tokens tokens
         result []]
    (cond
      (empty? tokens) result
      (and (is-literal (first tokens))
           (is-literal (last result)))
        (recur (rest tokens)
               (conj (pop result) (merge-literals (last result)
                                                  (first tokens))))
      :else
        (recur (rest tokens)
               (conj result (first tokens))))))

(defn cleanup-symbol
  [symbol-token]
  [:symbol (clojure.string/replace (last symbol-token) #"[\s\{\}]" "")])

(defn cleanup-iter-end
  [symbol-token]
  [:iter-end (clojure.string/replace (last symbol-token) #"[\s\{\}/]" "")])

(defn cleanup-iter-init
  [token]
  (let [parts (clojure.string/split (re-find #"\w+\s*\S+" (last token)) #"\s+")]
    (vector :iter-init (first parts) (last parts))))

(defn cleanup-tokens
  "Given a list of tokens, it will cleanup the
  tokens to nodes using the following rules:
  1. :literals do not change
  2. :symbols will be cleaned up e.g. [:symbol {{foo}}]
     to just [:symbol 'foo'"
  [tokens]
  (loop [tokens tokens
         result []]
    (if (empty? tokens)
      result
      (recur (rest tokens)
             (conj result
                   (let [token (first tokens)]
                     (cond
                       (is-literal token) token
                       (is-symbol token) (cleanup-symbol token)
                       (is-iter-init token) (cleanup-iter-init token)
                       (is-iter-end token) (cleanup-iter-end token))))))))

(defn parse
  [string]
  ((comp cleanup-tokens
         join-literals
         tokenize) string))

