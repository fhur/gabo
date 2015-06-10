(ns gabo.core-test
  (:require [clojure.test :refer :all]
            [gabo.core :refer :all]))

(defmacro expected
  "Simple expectation macro.
  Asserts that the operator applied to actual and expectation yields a truth result
  Example: (expected (+ 5 3) = 8)
           (expected (inc 2) = 3)"
  [actual operator expectation]
  `(testing (str "Expected " '~actual " to " '~operator " " '~expectation)
    (is (~operator ~actual
                   ~expectation))))

(defmacro expected-when
  "Creates a deftest with a group of 'expected' calls. It should
  be used to quickly test for several input cases to a given function.
  Syntax:
  (expected-when 'test-name' 'function'
    :when ['args'] 'operator' 'expectation'
    ...
    :when ['args'] 'operator' 'expectation')
  Example:
  (expected-when inc-test inc
    :when [0] = 1
    :when [1] = 2
    :when [3] = 3)"
  [test-name func & expectations]
  (let [test-name-sym (symbol test-name)]
    (concat `(deftest ~test-name-sym)
            (for [[_ args operator expected] (partition 4 expectations)]
              `(expected (apply ~func ~args) ~operator ~expected)))))

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

(deftest when-match-test
  (testing "Assigns the result of matching the
           string to to the symbol"
    (is (= "f"
           (when-match "foobar"
             [match #"^f"] match))))

  (testing "Several matches"
    (is (= ["xabcx" "abc"]
           (when-match " xabcx "
            [match #"x(\w+)x"] match))))

  (testing "Returns the first match in order"
    (is (= "pizza"
           (when-match "pizzapie"
             [match #"pizza"] match
             [match #"pie"] match)))))

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


