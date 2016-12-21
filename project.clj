(defproject endophile "0.2.0"
  :description "See README.md"
  :url "https://github.com/theJohnnyBrown/endophile/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.pegdown/pegdown "1.6.0"]
                 [enlive "1.1.6"]
                 [hiccup "1.0.5"]]
  :profiles {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0-alpha14"]]}}
  :aliases {"all" ["with-profile" "dev:dev,1.9:dev,1.7:dev,1.6"]})
