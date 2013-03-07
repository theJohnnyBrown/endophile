# endophile

A Clojure markdown parsing tool wrapping the java pegdown library. It is designed to convert markdown into clojure data structures, which can then be used to generate HTML through another library, such as enlive or hiccup.

## Usage

In project.clj, `:dependencies [[endophile "0.1.0"] ...]`

`endophile.core/md` takes a markdown string, and returns an
`org.pegdown.ast.RootNode`, the root of the document's parse tree.
`endophile.core/to-clj` converts a `RootNode` into a clojure data structure.
Currently it returns nodes as used in `clojure.xml` and in the enlive HTML library.

So for example,

```
(ns my-namespace
  (:use [endophile.core :only [mp to-clj]]
        [net.cgrand.enlive-html :only [emit*]]))

(def parsed (to-clj (mp (slurp "README.md"))))

(println (apply str (emit* parsed)))

```

Will convert README.md to html. Also you can run it from the command line,
like so,

`lein run -m endophile.core README.md > README.html`

It wouldn't be too hard to create a similar function that returns hiccup-style
vectors. See core.clj.

## TODO

Currently does not support reference-style links or images, abbreviations, or
tables

## License

Copyright © 2013 Jonathan Brown

Distributed under the Eclipse Public License, the same as Clojure.
