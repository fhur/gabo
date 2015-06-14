(ns gabo.core
  (:import (java.util ArrayList Stack)))

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

;; execute define-is-token-funcs to actually define the given functions
(define-is-token-funcs :literal :symbol :iter-init :iter-end :iter)

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
  "Given a string of code return a tuple of [match token]
  where match is the exact string that matched and token is
  the token that corresponds to the regex match.
  Example: (tokenize-chunk '{{bar}} foo') will return
  ['{{bar}}' [:symbol 'bar']]"
  [code]
  (when-match code
    [sym #"\A\{\{\s*([\w-\.]+)\s*\}\}"] [ (first sym) [:symbol (last sym) ] ]
    [iter-end #"\A\{\{/\s*(\w+)\s*\}\}"] [ (first iter-end) [:iter-end (last iter-end) ]]
    [iter-init #"\A\{\{#\s*(\w+)\s*\}\}"] [ (first iter-init) [:iter-init (last iter-init) :default ]]
    [iter-init #"\A\{\{#\s*(\w+)\s+(\S+?)\s*\}\}"] [(first iter-init) (cons :iter-init (rest iter-init))]
    [literal #"\A([\s\S][\s\S]*?)\{\{"] [(last literal) [:literal (last literal)]]
    [literal #"\A[\s\S]*"] [literal [:literal literal]]))

(defn tokenize
  [code]
  (loop [code code
         tokens []]
    (if (empty? code)
      tokens
      (let [[str-match token] (tokenize-chunk code)]
        (recur (.substring code (count str-match))
               (conj tokens token))))))

(defn add! [array-list element]
  (.add array-list element)
  array-list)

(defn persist-tree
  "Given a mutable list of nodes, converts it to a immutable tree"
  [tree]
  (cond
    (or (is-literal tree) (is-symbol tree))
      (vec tree)
    (is-iter tree)
      (let [[token-type sym separator sub-tree] tree]
        (vector token-type sym separator (map persist-tree sub-tree)))
    :else
      (map persist-tree tree)))


(defn build-ast
  [tokens]
  ;; NOTE: until I can figure out how to implement this
  ;; using persistent data structured, good ol' java will
  ;; have to do.
  (let [ast (ArrayList.)
        stack (java.util.Stack.)]
    (.push stack ast) ; initialize the stack
    (doseq [token tokens]
      (cond
        (or (is-literal token) (is-symbol token))
          (add! (.peek stack) token)
        (is-iter-init token)
          (let [mutable-token (ArrayList. [:iter (second token) (last token)])]
            (add! mutable-token (ArrayList.))
            (add! (.peek stack) mutable-token)
            (.push stack (last mutable-token)))
        (is-iter-end token)
          (.pop stack)))
    ast))

(defn parse
  [string]
  ((comp persist-tree
         build-ast
         tokenize) string))

(defn eval-tree
  "Evaluates a compiled template as a tree with the given context"
  [tree ctx]
  (cond (is-literal tree)
          (second tree)
        (is-symbol tree)
          (let [[_ sym] tree]
            (if (= sym ".")
              (str ctx)
              (get ctx (keyword sym) "")))
        (is-iter tree)
          (apply str
                 (let [[_ sym separator sub-tree] tree ]
                   (interpose (if (= :default separator) "," separator)
                              (for [item (ctx (keyword sym))]
                                (eval-tree sub-tree item)))))
        (coll? tree)
          (apply str (map #(eval-tree % ctx) tree))))

(defn render
  "Compiles and evaluates the template with the given context"
  [template ctx]
  (eval-tree (parse template) ctx))
