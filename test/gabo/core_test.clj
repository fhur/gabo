(ns gabo.core-test
  (:require [clojure.test :refer :all]
            [gabo.core :refer :all]
            [presto.core :refer :all]))

;; Test the is-literal, is-symbol, etc. functions
(deftest is-functions
  (expected (is-literal [:literal "foo"]) = true)
  (expected (is-symbol [:symbol "arg"]) = true)
  (expected (is-iter-init [:iter-init "bar"]) = true)
  (expected (is-iter-init [:iter-init "bar" ","]) = true)
  (expected (is-iter-end [:iter-end "asdf"]) = true)
  (testing "All should return false on nil arg"
    (is (= (repeat 4 false)
           (map #(% nil) [is-literal is-symbol
                          is-iter-init is-iter-end])))))

(expected-when "tokenize-chunk-test" tokenize-chunk
  :when ["{{n}}"] = [ "{{n}}" [:symbol "n"]]
  :when ["{{foo}}"] = [ "{{foo}}" [:symbol "foo"]]
  :when ["{{ foo }}"] = [ "{{ foo }}" [:symbol "foo"]]
  :when ["{{ foo}}"] = [ "{{ foo}}" [:symbol "foo"]]
  :when ["{{foo }}"] = [ "{{foo }}" [:symbol "foo"]]
  :when ["{{foo-bar}}"] = ["{{foo-bar}}" [:symbol "foo-bar"]]
  :when ["{{ foo-bar }}"] = ["{{ foo-bar }}" [:symbol "foo-bar"]]
  :when ["{{ foo-bar-baz }}"] = ["{{ foo-bar-baz }}" [:symbol "foo-bar-baz"]]
  :when ["{{ . }}"] = ["{{ . }}" [:symbol "."]])

(expected-when "tokenize-chunk-iter-end" tokenize-chunk
  :when ["{{/ foo }}"] = [ "{{/ foo }}" [:iter-end "foo"]]
  :when ["{{/foo }}"] = [ "{{/foo }}" [:iter-end "foo"]]
  :when ["{{/ foo}}"] = [ "{{/ foo}}" [:iter-end "foo"]]
  :when ["{{/foo}}"] = [ "{{/foo}}" [:iter-end "foo"]])

(expected-when "tokenize-chunk-iter-init" tokenize-chunk
  :when ["{{#foo}}"] = [ "{{#foo}}" [:iter-init "foo" :default]]
  :when ["{{# foo}}"] = [ "{{# foo}}" [:iter-init "foo" :default]]
  :when ["{{#foo }}"] = [ "{{#foo }}" [:iter-init "foo" :default]]
  :when ["{{# foo }}"] = [ "{{# foo }}" [:iter-init "foo" :default]]
  :when ["{{#foo ,}}"] = [ "{{#foo ,}}" [:iter-init "foo" ","]]
  :when ["{{#foo , }}"] = [ "{{#foo , }}" [:iter-init "foo" ","]]
  :when ["{{# foo , }}"] = [ "{{# foo , }}" [:iter-init "foo" ","]]
  :when ["{{#foo sep}}"] = [ "{{#foo sep}}" [:iter-init "foo" "sep"]]
  :when ["{{# foo sep }} "] = [ "{{# foo sep }}" [:iter-init "foo" "sep"]])

(expected-when "tokenize-literals" tokenize-chunk
  :when ["asdfasdf"] = [ "asdfasdf" [:literal "asdfasdf"]]
  :when ["asdf {{foo}}"] = [ "asdf " [:literal "asdf "]]
  :when ["as\ndf "] = [ "as\ndf " [:literal "as\ndf "]]
  :when ["as\ndf {{foo}}"] = [ "as\ndf " [:literal "as\ndf "]])

(expected-when "tokenize-test" tokenize
  :when [""] = []
  :when ["lit"] = [[:literal "lit"]]
  :when ["{{#loop}}"] = [[:iter-init "loop" :default]]
  :when ["{{# loop ,}}"] = [[:iter-init "loop" ","]]
  :when ["{{/loop}}"] = [[:iter-end "loop"]]
  :when ["foo {{a}}{{#loop}} asdf {{n}} {{/loop}}"] =
        [[:literal "foo "] [:symbol "a"] [:iter-init "loop" :default] [:literal " asdf "] [:symbol "n"]
        [:literal " "] [:iter-end "loop"]]
  :when ["{{# a ,}}{{b}}{{#c}}{{d}}{{/c}}{{/a}}"] =
        [[:iter-init "a" ","] [:symbol "b"] [:iter-init "c" :default] [:symbol "d"] [:iter-end "c"] [:iter-end "a"]])

(expected-when "render-test" render
  :when ["" {}] = ""
  :when ["literal" {}] = "literal"
  :when ["lit\nera\nl" {:foo "bar"}] = "lit\nera\nl"
  :when ["Hi {{name}}, how are you?" {:name "Bob"}] = "Hi Bob, how are you?"
  :when ["name: {{name}}, last name {{last-name}}" {:name "Bob" :last-name "Doe"}] = "name: Bob, last name Doe"
  :when ["Hi {{name}}, your friends are{{# friends ,}} {{.}}{{/friends}}" {:name "Bob" :friends ["Frank" "Charlie"]}]
        = "Hi Bob, your friends are Frank, Charlie"
  :when ["People:{{#items}} {{name}}: {{age}}{{/items}}"
         {:items [{:name "Bob" :age 15}
                  {:name "Frank" :age 23}
                  {:name "Ann" :age 12}]}]
        = "People: Bob: 15, Frank: 23, Ann: 12")


(deftest parse-test
  (testing "The empty program"
    (is (= []
           (parse ""))))

  (testing "A simple template with no iters"
    (is (= [[:literal "My name is:"] [:symbol "name"] [:literal ", slim shady"]]
           (parse "My name is:{{name}}, slim shady"))))

  (testing "A template with an :iter"
    (is (= [[:literal "Your name is"] [:symbol "name"] [:literal " and your friends are "]
            [:iter "friends" "," [ [:literal " "] [:symbol "name"] [:literal " "] ]]]
           (parse "Your name is{{name}} and your friends are {{#friends , }} {{name}} {{/friends}}"))))

  (testing "Simple iter"
    (is (= [[:iter "loop" "sep" [[:literal " literal "]]]]
           (parse "{{# loop sep}} literal {{/loop}}"))))

  (testing "A template with nested :iters"
    (is (= (parse "{{#a}}{{#b}}{{c}}{{/b}}{{/a}}")
           [[:iter "a" :default
            [[:iter "b" :default [[:symbol "c"]]]]]]))))



