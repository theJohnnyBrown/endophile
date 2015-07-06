(ns endophile.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [endophile.core :refer :all]
            [hiccup.core :refer [html]]
            [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [endophile.hiccup :as md2h]
            [endophile.utils :refer :all])
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

(def markdown-spec-files
  (->> (file-seq (io/file test-files-dir))
       (map #(.getPath %))
       (filter #(re-find #"\.text$" %))
       (map (fn [path] [path (str/replace path #"\.text$" ".html")]))))

(deftest test-to-clj
  (doseq [[md-file html-file] markdown-spec-files]
    (is
     (=
      (tidy (html/html-snippet (slurp html-file)))
      (tidy (html/html-snippet (html-string (to-clj (mp (slurp md-file)))))))
     (str "Testing enlive: " md-file))))

(deftest test-hiccup
  (doseq [[md-file html-file] markdown-spec-files]
    (is
     (=
      (tidy (html/html-snippet (slurp html-file)))
      (tidy (html/html-snippet (html (md2h/to-hiccup (mp (slurp md-file)))))))
     (str "Testing hiccup: " md-file))))

(deftest test-img-tag
  (let [parsed (mp "![alt text](/image/url \"image title\")")
        result (to-clj parsed)]
    (is (= [{:tag :p :content [{:tag :img :attrs {:src "/image/url" :alt "alt text" :title "image title"}}]}]
           result))))

(deftest test-reference-style-link-inside-list
  (is (= [{:tag :ul,
           :content
           [{:tag :li,
             :content
             ["List " {:tag :a, :attrs {:href "link"}, :content ["item"]}]}]}
          ""]
         (to-clj (mp "* List [item][]\n\n[item]: link")))))

(deftest mp-options
  (is (= [{:tag :p :content ["~" "~" "foo" "~" "~"]}]
         (to-clj (mp "~~foo~~" {:extensions {:strikethrough false}})))))

(deftest anchorlinks-test
  (is (= [{:tag :h1, :content [{:tag :a, :attrs {:name "foo" :href "#foo"}, :content ["Foo"]}]}]
         (to-clj (mp "# Foo" {:extensions {:anchorlinks true}}))))
  (is (= [[:h1 [:a {:name "foo" :href "#foo"} "Foo"]]]
         (md2h/to-hiccup (mp "# Foo" {:extensions {:anchorlinks true}})))))
