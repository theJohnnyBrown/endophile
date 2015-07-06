(ns endophile.core-test
  (:use clojure.test
        clojure.java.io
        endophile.core
        endophile.utils
        [hiccup.core :only [html]])
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [endophile.hiccup :as md2h])
  (:import [org.pegdown Extensions]))

(def default-extensions (bit-or Extensions/AUTOLINKS Extensions/FENCED_CODE_BLOCKS Extensions/STRIKETHROUGH))

(deftest extensions-test
  (testing "default extensions"
    (is (= default-extensions
           (extensions-map->int {}))))
  (testing "disabling defaults"
    (is (= (bit-and-not default-extensions Extensions/FENCED_CODE_BLOCKS Extensions/STRIKETHROUGH)
           (extensions-map->int {:fenced-code-blocks false
                                 :strikethrough false}))))
  (is (= (bit-or default-extensions Extensions/SMARTS)
         (extensions-map->int {:smarts true}))))

(def test-files-dir "test/resources/")
(defn markdown-files [files]
  (filter
   (fn [ff] (= (subs (.getName ff) (- (count (.getName ff)) 4)) "text"))
   files))
(def markdown-spec-files
  (for [md-file (-> test-files-dir file .listFiles markdown-files)]
    (let [md-file (.getPath md-file)]
      [md-file (str/replace md-file ".text" ".html")])))

(defn parsed= [& html-strings]
  (apply = (map html/html-snippet html-strings)))

(deftest test-to-clj
  (doseq [[md-file html-file] markdown-spec-files]
    (is
     (=
      (str/trim (tidy (html-string (to-clj (mp (slurp md-file))))))
      (str/trim (tidy (slurp html-file))))
     (str "Testing enlive: " md-file))))

(deftest test-hiccup
  (doseq [[md-file html-file] markdown-spec-files]
    (is
     (parsed=
      (str/trim (tidy (html (md2h/to-hiccup (mp (slurp md-file))))))
      (str/trim (tidy (slurp html-file))))
     (str "Testing hiccup: " md-file))))

(deftest test-img-tag
  (let [parsed (mp "![alt text](/image/url \"image title\")")
        result (to-clj parsed)]
    (is (= result [{:tag :p :content [{:tag :img :attrs {:src "/image/url" :alt "alt text" :title "image title"}}]}]))))

(deftest test-reference-style-link-inside-list
  (is (= (to-clj (mp "* List [item][]\n\n[item]: link"))
         [{:tag :ul,
           :content
           [{:tag :li,
             :content
             ["List " {:tag :a, :attrs {:href "link"}, :content ["item"]}]}]}
          ""])))

(deftest mp-options
  (is (= [{:tag :p :content ["~" "~" "foo" "~" "~"]}]
         (to-clj (mp "~~foo~~" {:extensions {:strikethrough false}})))))
