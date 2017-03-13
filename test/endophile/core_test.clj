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

(deftest email-test
  (is (= [{:tag :p, :content [{:tag :a, :attrs {:href "mailto:juho@metosin.fi"}, :content ["juho@metosin.fi"]}]}]
         (to-clj (mp "<juho@metosin.fi>"))))
  (is (= [[:p [:a {:href "mailto:juho@metosin.fi"} "juho@metosin.fi"]]]
         (md2h/to-hiccup (mp "<juho@metosin.fi>"))))
  (is (= [{:tag :p, :content [{:tag :a, :attrs {:href "mailto:juho@metosin.fi"}, :content ["juho@metosin.fi"]}]}]
         (to-clj (mp "juho@metosin.fi"))))
  (is (= [[:p [:a {:href "mailto:juho@metosin.fi"} "juho@metosin.fi"]]]
         (md2h/to-hiccup (mp "juho@metosin.fi")))) )

(deftest tables-test
  (let [table-md (mp (str "| A | B \n"
                          "| - | - |\n"
                          "| C | D |\n"
                          "[Caption]")
                     {:extensions {:tables true}})]
    (is (= [{:tag :table
             :content
             [{:tag :thead
               :content
               [{:tag :tr
                 :content [{:tag :th, :content ["A "]}
                           {:tag :th, :content ["B"]}]}]}
              {:tag :tbody
               :content
               [{:tag :tr
                 :content [{:tag :td, :content ["C "]}
                           {:tag :td, :content ["D "]}]}]}
              {:tag :caption
               :content ["Caption"]}]}]
           (to-clj table-md)))
    (is (= [[:table
             [:thead [:tr [:th "A "] [:th "B"]]]
             [:tbody [:tr [:td "C "] [:td "D "]]]
             [:caption "Caption"]]]
           (md2h/to-hiccup table-md))))

  (let [headless-md (mp (str "| -------- |\n"
                             "| headless |\n")
                        {:extensions {:tables true}})]
    (is (= [{:tag :table
             :content [{:tag :tbody
                        :content
                        [{:tag :tr
                          :content [{:tag :td, :content ["headless "]}]}]}]}]
           (to-clj headless-md)))
    (is (= [[:table [:tbody [:tr [:td "headless "]]]]]
           (md2h/to-hiccup headless-md))))

  (let [column-md (mp (str "|     | GROUP   ||      |\n"
                           "| HE  | AD  | ER | S    |\n"
                           "|:--- | ---:| -- |:----:|\n"
                           "| A   | B   | CCC      ||\n"
                           "| D   | E   |  F | GG   |\n"
                           "| HH  |  I      ||  J   |\n"
                           "[Caption]")
                      {:extensions {:tables true}})]
    (is (= [{:tag :table
             :content
             [{:tag :thead
               :content
               [{:tag :tr
                 :content
                 [{:tag :th,  :content [" "], :attrs {:alignment "left"}}
                  {:tag :th, :content ["GROUP "]
                   :attrs {:colspan 2, :alignment "right"}}
                  {:tag :th, :content [" "], :attrs {:alignment "center"}}]}
                {:tag :tr
                 :content
                 [{:tag :th, :content ["HE "], :attrs {:alignment "left"}}
                  {:tag :th, :content ["AD "], :attrs {:alignment "right"}}
                  {:tag :th, :content ["ER "]}
                  {:tag :th, :content ["S "], :attrs {:alignment "center"}}]}]}
              {:tag :tbody
               :content
               [{:tag :tr
                 :content
                 [{:tag :td, :content ["A "], :attrs {:alignment "left"}}
                  {:tag :td, :content ["B "], :attrs {:alignment "right"}}
                  {:tag :td, :content ["CCC "], :attrs {:colspan 2}}]}
                {:tag :tr
                 :content
                 [{:tag :td, :content ["D "], :attrs {:alignment "left"}}
                  {:tag :td, :content ["E "], :attrs {:alignment "right"}}
                  {:tag :td, :content ["F "]}
                  {:tag :td, :content ["GG "], :attrs {:alignment "center"}}]}
                {:tag :tr
                 :content
                 [{:tag :td, :content ["HH "], :attrs {:alignment "left"}}
                  {:tag :td, :content ["I "]
                   :attrs {:colspan 2, :alignment "right"}}
                  {:tag :td, :content ["J "], :attrs {:alignment "center"}}]}]}
              {:tag :caption, :content ["Caption"]}]}]
           (to-clj column-md)))
    (is (= [[:table
             [:thead
              [:tr
               [:th {:alignment "left"} " "]
               [:th {:colspan 2, :alignment "right"} "GROUP "]
               [:th {:alignment "center"} " "]]
              [:tr
               [:th {:alignment "left"} "HE "]
               [:th {:alignment "right"} "AD "]
               [:th "ER "]
               [:th {:alignment "center"} "S "]]]
             [:tbody
              [:tr
               [:td {:alignment "left"} "A "]
               [:td {:alignment "right"} "B "]
               [:td {:colspan 2} "CCC "]]
              [:tr [:td {:alignment "left"} "D "]
               [:td {:alignment "right"} "E "]
               [:td "F "]
               [:td {:alignment "center"} "GG "]]
              [:tr
               [:td {:alignment "left"} "HH "]
               [:td {:colspan 2, :alignment "right"} "I "]
               [:td {:alignment "center"} "J "]]]
             [:caption "Caption"]]]
           (md2h/to-hiccup column-md)))))
