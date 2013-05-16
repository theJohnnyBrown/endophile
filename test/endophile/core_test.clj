(ns endophile.core-test
  (:use clojure.test
        clojure.java.io
        endophile.core)
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]))

(def test-files-dir "test/resources/")
(defn markdown-files [files]
  (filter
   (fn [ff] (= (subs (.getName ff) (- (count (.getName ff)) 4)) "text"))
   files))


(deftest a-test
  (doseq [md-file (-> test-files-dir file .listFiles markdown-files)]
   (is
    (= (str/trim (tidy (apply str (html/emit* (to-clj (mp (slurp md-file)))))))
       (str/trim
        (tidy (slurp (str
                      test-files-dir
                      (str/replace (.getName md-file)
                                   ".text" ".html"))))))
    (str "Testing: " (.getName md-file)))))
