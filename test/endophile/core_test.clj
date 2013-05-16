(ns endophile.core-test
  (:use clojure.test
        clojure.java.io
        endophile.core)
  (:import [java.io StringWriter StringReader]
           [org.w3c.tidy Tidy])
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]))

(def test-files-dir "test/resources/")
(defn markdown-files [files]
  (filter
   (fn [ff] (= (subs (.getName ff) (- (count (.getName ff)) 4)) "text"))
   files))

(defn tidy [untidy]
  (let [w (StringWriter.)]
    (doto (Tidy.)
      (.setTabsize 4)
      (.setPrintBodyOnly true)
      (.setShowWarnings false)
      (.setQuiet true)
      (.parse (StringReader. untidy) w))
    (str/replace (.toString w) "\r\n" "\n")))

(deftest a-test
  (doseq [md-file (-> test-files-dir file .listFiles markdown-files)]
   (is
    (= (tidy (apply str (html/emit* (to-clj (mp (slurp md-file))))))
       (tidy (slurp (str
                       test-files-dir
                       (str/replace (.getName md-file)
                                    ".text" ".html")))))
    (str "Testing: " (.getName md-file)))))
