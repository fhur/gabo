(ns gabo.util)

(defmacro define-is-token-funcs
  "Given a list of keywords k1, k2, ..., kn
  this macro will define functions is-k1, is-k2, ... is-kn
  where each one takes as input a token and returns true if the
  token matches the given type
  Example: (define-is-token-funcs :foo :bar)
  will define two functions (is-foo [token]) and (is-bar [token]) which
  test that the token is of type :foo and :bar respectively."
  [& token-types]
  (cons 'do
         (map (fn [token-type]
                `(defn ~(symbol (str "is-" (name token-type)))
                   ~(str "Returns true if the given token is of type " token-type ", false otherwise")
                  [token#] (= (first token#) ~token-type)))
              token-types)))

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

