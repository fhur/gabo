(ns gabo.util-test
  (:require [clojure.test :refer :all]
            [presto.core :refer :all]
            [gabo.util :refer :all]))

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
             [match #"pie"] match))))

  (testing "Can apply a function to the matched argument"
    (is (= :foo
           (when-match "boofoo"
             [match #"foo"] (keyword match)))))
    (is (= "foobar"
           (when-match "loofootl"
             [m #"foo"] (str m "bar")))))



