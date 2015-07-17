(ns gabo.lexer
  (:require [gabo.util :refer :all]))

(defn tokenize-chunk
  "Given a string of code return a tuple of [match token]
  where match is the exact string that matched and token is
  the token that corresponds to the regex match.
  Example: (tokenize-chunk '{{bar}} foo') will return
  ['{{bar}}' [:symbol 'bar']]"
  [code]
  (when-match code
    ;; match symbols e.g. {{foo}}
    [sym #"\A\{\{\s*([\w-\.]+)\s*\}\}"]
      [ (first sym) [:symbol (last sym) ] ]

    ;; match iter-end e.g. {{/foo}}
    [iter-end #"\A\{\{/\s*(\w+)\s*\}\}"]
      [ (first iter-end) [:iter-end (last iter-end) ]]

    ;; match iter-init with no args e.g. {{#foo}}
    [iter-init #"\A\{\{#\s*(\w+)\s*\}\}"]
      [ (first iter-init) [:iter-init (last iter-init) :default ]]

    ;; match iter-init with args e.g. {{#foo 'separator'}}
    [iter-init #"\A\{\{#\s*(\w+)\s+'([\s\S]*?)'\s*\}\}"]
      [(first iter-init) (cons :iter-init (rest iter-init))]

    ;; match literals
    [literal #"\A([\s\S][\s\S]*?)\{\{"]
      [(last literal) [:literal (last literal)]]

    ;; more literals
    [literal #"\A[\s\S]*"]
      [literal [:literal literal]]))

(defn tokenize
  [code]
  (loop [code code
         tokens []]
    (if (empty? code)
      tokens
      (let [[str-match token] (tokenize-chunk code)]
        (recur (.substring code (count str-match))
               (conj tokens token))))))


