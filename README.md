# endophile

[![Build Status](https://secure.travis-ci.org/theJohnnyBrown/endophile.png)](http://travis-ci.org/theJohnnyBrown/endophile)

A Clojure markdown parsing tool wrapping java's [pegdown](https://github.com/sirthias/pegdown) library. It is designed to convert markdown into clojure data structures, which can then be used to generate HTML through another library, such as enlive or hiccup.

Endophile fully passes the original markdown test suite.

## Usage

In project.clj, `:dependencies [[endophile "0.1.2"] ...]`

`endophile.core/md` takes a markdown string, and returns an
`org.pegdown.ast.RootNode`, the root of the document's parse tree.
`endophile.core/to-clj` converts a `RootNode` into a clojure data structure.
Currently it returns nodes as used in `clojure.xml` and in the enlive HTML library.

So for example,

```clojure
(ns my-namespace
  (:use [endophile.core :only [mp to-clj html-string]]
        [endophile.hiccup :only [to-hiccup]]
        [hiccup.core :only [html]]))

;; parsed is an org.pegdown.RootNode
(def parsed (mp (slurp "README.md")))

;; convert to html using clojure.xml and enlive
(println (html-string (to-clj parsed)))

;; convert to html using hiccup
(println (html (to-hiccup parsed)))

```

Will convert README.md to html. Also you can run it from the command line,
like so,

`lein run -m endophile.core README.md > README.html`

There is also an implementation that returns hiccup-style vectors. See hiccup.clj.

## TODO

Currently does not support the full complement of extensions available through pegdown. Pull requests are encouraged.

## License

Copyright © 2013 Jonathan Brown

Distributed under the Eclipse Public License, the same as Clojure.
