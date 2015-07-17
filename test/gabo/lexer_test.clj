(ns gabo.lexer-test
  (:require [clojure.test :refer :all]
            [gabo.lexer :refer :all]
            [presto.core :refer :all]))

(expected-when "test tokenize symbols" tokenize-chunk
  :when ["{{n}}"] = [ "{{n}}" [:symbol "n"]]
  :when ["{{foo}}"] = [ "{{foo}}" [:symbol "foo"]]
  :when ["{{ foo }}"] = [ "{{ foo }}" [:symbol "foo"]]
  :when ["{{ foo}}"] = [ "{{ foo}}" [:symbol "foo"]]
  :when ["{{foo }}"] = [ "{{foo }}" [:symbol "foo"]]
  :when ["{{foo-bar}}"] = ["{{foo-bar}}" [:symbol "foo-bar"]]
  :when ["{{ foo-bar }}"] = ["{{ foo-bar }}" [:symbol "foo-bar"]]
  :when ["{{ foo-bar-baz }}"] = ["{{ foo-bar-baz }}" [:symbol "foo-bar-baz"]]
  :when ["{{ . }}"] = ["{{ . }}" [:symbol "."]])

(expected-when "tokenize iter-end tokens" tokenize-chunk
  :when ["{{/ foo }}"] = [ "{{/ foo }}" [:iter-end "foo"]]
  :when ["{{/foo }}"] = [ "{{/foo }}" [:iter-end "foo"]]
  :when ["{{/ foo}}"] = [ "{{/ foo}}" [:iter-end "foo"]]
  :when ["{{/foo}}"] = [ "{{/foo}}" [:iter-end "foo"]])

(expected-when "tokenize iter-init tokens" tokenize-chunk
  :when ["{{#foo}}"] = [ "{{#foo}}" [:iter-init "foo" :default]]
  :when ["{{# foo}}"] = [ "{{# foo}}" [:iter-init "foo" :default]]
  :when ["{{#foo }}"] = [ "{{#foo }}" [:iter-init "foo" :default]]
  :when ["{{# foo }}"] = [ "{{# foo }}" [:iter-init "foo" :default]]
  :when ["{{#foo ','}}"] = [ "{{#foo ','}}" [:iter-init "foo" ","]]
  :when ["{{#foo ',' }}"] = [ "{{#foo ',' }}" [:iter-init "foo" ","]]
  :when ["{{# foo ',' }}"] = [ "{{# foo ',' }}" [:iter-init "foo" ","]]
  :when ["{{#foo 'sep'}}"] = [ "{{#foo 'sep'}}" [:iter-init "foo" "sep"]]
  :when ["{{# foo 'sep' }} "] = [ "{{# foo 'sep' }}" [:iter-init "foo" "sep"]]
  :when ["{{#foo '\n'}}"] = ["{{#foo '\n'}}" [:iter-init "foo" "\n"]])

(expected-when "tokenize literals" tokenize-chunk
  :when ["asdfasdf"] = [ "asdfasdf" [:literal "asdfasdf"]]
  :when ["asdf {{foo}}"] = [ "asdf " [:literal "asdf "]]
  :when ["as\ndf "] = [ "as\ndf " [:literal "as\ndf "]]
  :when ["as\ndf {{foo}}"] = [ "as\ndf " [:literal "as\ndf "]])

(expected-when "tokenize-test" tokenize
  :when [""] = []
  :when ["lit"] = [[:literal "lit"]]
  :when ["{{#loop}}"] = [[:iter-init "loop" :default]]
  :when ["{{# loop ','}}"] = [[:iter-init "loop" ","]]
  :when ["{{/loop}}"] = [[:iter-end "loop"]]
  :when ["foo {{a}}{{#loop}} asdf {{n}} {{/loop}}"] =
        [[:literal "foo "] [:symbol "a"] [:iter-init "loop" :default] [:literal " asdf "] [:symbol "n"]
        [:literal " "] [:iter-end "loop"]]
  :when ["{{# a ','}}{{b}}{{#c}}{{d}}{{/c}}{{/a}}"] =
        [[:iter-init "a" ","] [:symbol "b"] [:iter-init "c" :default] [:symbol "d"] [:iter-end "c"] [:iter-end "a"]])
