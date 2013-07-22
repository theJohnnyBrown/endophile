(ns endophile.core-test
  (:use clojure.test
        clojure.java.io
        endophile.core
        endophile.utils
        [hiccup.core :only [html]])
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [endophile.hiccup :as md2h]))

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
