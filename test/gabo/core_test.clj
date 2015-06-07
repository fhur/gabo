(ns gabo.core-test
  (:require [clojure.test :refer :all]
            [gabo.core :refer :all]))


(defmacro cleanup-iter-init-test-case
  "Returns a (testing ...) form that asserts that
  (is (= (cleanup-iter-init [:iter-init match])
         [:iter-init exp1 exp2]))"
  [match exp1 exp2]
  `(testing (str "cleanup-iter-init (" ~match "=> " ~exp1 " " ~exp2 ")")
     (is (= (cleanup-iter-init [:iter-init ~match])
            [:iter-init ~exp1 ~exp2]))))

(deftest is-functions
  (testing "is-literal"
    (is (is-literal [:literal "foo"])))
  (testing "is-symbol"
    (is (is-symbol [:symbol "arg"])))
  (testing "is-iter-init"
    (is (is-iter-init [:iter-init "bar"])))
  (testing "is-iter-end"
    (is (is-iter-end [:iter-end "asdf"])))
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
  (testing "Returns the first match in order"
    (is (= "pizza"
           (when-match "pizzapie"
             [match #"pizza"] match
             [match #"pie"] match)))))

(deftest tokenize-test
  (testing "The empty program"
    (is (= []
           (tokenize ""))))
  (testing "Simple literal tokenization"
    (is (= [[:literal "lit"]]
           (tokenize "lit"))))
  (testing "iter-init tokenization"
    (is (= [[:iter-init "{{#loop}}"]]
           (tokenize "{{#loop}}"))))
  (testing "iter-init tokenization with separator"
    (is (= [[:iter-init "{{# loop ,}}"]]
           (tokenize "{{# loop ,}}"))))
  (testing "iter-end tokenization"
    (is (= [[:iter-end "{{/loop}}"]]
           (tokenize "{{/loop}}"))))
  (testing "Simple symbol tokenization"
    (is (= [:literal :symbol :literal :iter-init
            :literal :symbol :literal
            :iter-end]
           ;; the current implementation of tokenize sometimes separates literals instead of joining
           ;; them into the same token. join-literals joins all conseq literals
           (map first (join-literals (tokenize "foo {{a}} {{#loop}} asdf {{n}} {{/loop}}")))))))


(deftest cleanup-iter-init-test
  (cleanup-iter-init-test-case "{{# foo}}"   "foo" ",")
  (cleanup-iter-init-test-case "{{# foo ,}}" "foo" ",")
  (cleanup-iter-init-test-case "{{# foo sep }}" "foo" "sep")
  (cleanup-iter-init-test-case "{{#foo sep }}" "foo" "sep")
  (cleanup-iter-init-test-case "{{# foo sep}}" "foo" "sep"))


(deftest parse-test
  (testing "The empty program"
    (is (= []
           (parse ""))))
  (testing "A simple template with no iters"
    (is (= [[:literal "My name is:"] [:symbol "name"] [:literal ", slim shady"]]
           (parse "My name is:{{name}}, slim shady"))))
  (testing "A template with an iter"
    (is (= [[:literal "Your name is"] [:symbol "name"] [:literal " and your friends are "]
            [:iter "friends" "," [ [:literal " "] [:symbol "name"] [:literal " "] ]]]
           (parse "Your name is{{name}} and your friends are {{#friends , }} {{name}} {{#friends}}")))))


