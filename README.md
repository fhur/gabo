# gabo

A minimal templating language for learning purposes only.

## Usage

First things first:
```clojure
(use 'gabo.core)
```

There are only 3 functions you should be using:

#### `(render template context)`
Renders a template with the given context. See the template syntax section on how to write templates.
#### `(parse template)`
Parses and compiles a template. The result is a tree representation of the template which can later be used
by `eval-tree`
#### `(eval-tree tree context)`
Evaluates a tree as produced by `parse` with the given context. The difference between `render` and `eval-tree`
is that `render` compiles a template every single time while `eval-tree` reuses the compiled template.

## Template syntax

Templates are written in syntax very similar to mustache templates.

Example:

```clojure
(def template
"<p> Hi {{name}} Your friends are: </p>
<ol>
{{#friends '\n'}}<li>{{.}}</li>{{/friends}}
</ol>")

(render template {:name "Bob" :friends ["Billy", "Ann", "Rob", "Sue"]})
```

#### Syntax:

##### Symbols `{{somevar}}`
Symbols are replaced with the textual representation of whatever you
supply are context.

Example:
```clojure
user=> (render "Hi {{name}}" {:name "Charly"})
"Hi Charly"
```

##### Iterating `{{#coll}} body {{/coll}}`
To iterate over a collection you need to specify the name of the
collection and you can supply an optional separator argument as `{{#coll 'foo'}}`.

Example:
```clojure
user=> (render "{{#numbers}}{{.}}{{/numbers}}" {:numbers [1 2 3 4 5]})
"1,2,3,4,5"
user=> (render "{{#numbers '\n'}}{{.}}{{/numbers}}" {:numbers [1 2 3 4 5]})
"1\n2\n3\n4\n5"
```

As you can see, the default separator is ',' but you can supply your own
separator.

You can also supply a map as the elements of the collection being
iterated:
```clojure
user=> (render "{{#friends '\n'}}Name: {{name}}, age {{age}}{{/friends}}"
               {:friends [{:name "Bob" :age 20}
                          {:name "Betty" :age 25}]})
"Name: Bob, age 20\nName: Betty, age 25"
```


## License

Copyright © 2015 Fernando Hurtado

Distributed under the Eclipse Public License, see [LICENCE](./LICENSE)
