(ns gabo.core
  (:require [gabo.util :refer :all]
            [gabo.lexer :refer :all]))

(defn- unexpected-token-exception
  [token]
  (new IllegalArgumentException
       (str "Unexpected token " token)))

;; execute define-is-token-funcs to actually define the given functions:
;; is-literal, is-symbol, etc.
(define-is-token-funcs :literal :symbol :iter-init :iter-end :iter)

(defn- find-iter-sub-list
  "Returns all tokens between an :iter-init and corresponding closing
  :iter-end pair."
  [tokens]
  {:pre [(is-iter-init (first tokens))]}
  (loop [remaining-tokens (rest tokens)
         sub-list []
         stack 0]
    (let [token (first remaining-tokens)]
      (if (and (zero? stack) (is-iter-end token))
        sub-list
        (recur (rest remaining-tokens)
               (conj sub-list token)
               (cond (is-iter-init token) (inc stack)
                     (is-iter-end token)  (dec stack)
                     :else                stack))))))

(defn- build-ast
  "Builds an abstract syntax tree given a list of tokens as produced by tokenize"
  [tokens]
  (loop [tokens tokens
         ast []]
    (if (empty? tokens)
      ast
      (let [token (first tokens)]
        (cond
          (or (is-literal token) (is-symbol token))
            (recur (rest tokens)
                   (conj ast token))
          (is-iter-init token)
            (let [sub-list (find-iter-sub-list tokens)
                  [_ token-val separator] token]
              (recur (drop (+ 2 (count sub-list)) tokens)
                     (conj ast [:iter token-val
                                      separator
                                      (build-ast sub-list)])))
        :else
          (throw (unexpected-token-exception token)))))))

(defn parse
  "Parses a template string and returns a compiled tree representation of the template.
  You can later use (eval-tree tree context) to render a compiled template with a given
  context."
  [string]
  (-> (tokenize string)
      build-ast))

(defn eval-tree
  "Evaluates a compiled template as a tree with the given context"
  [tree ctx]
  (cond (is-literal tree)
          (second tree)
        (is-symbol tree)
          (let [[_ token-val] tree]
            (if (= token-val ".")
              (str ctx)
              (get ctx (keyword token-val) "")))
        (is-iter tree)
          (let [[_ token-val separator sub-tree] tree
                coll (get ctx (keyword token-val) [])]
            (->> (map #(eval-tree sub-tree %) coll)
                 (interpose (if (= :default separator) "," separator))
                 (apply str)))
        ;; Only executed once: the first time eval-tree is called, no subsequent
        ;; recursive call will go through this branch.
        (coll? tree)
          (apply str (map #(eval-tree % ctx) tree))))

(defn render
  "Compiles and evaluates the template with the given context"
  [template ctx]
  (eval-tree (parse template) ctx))
