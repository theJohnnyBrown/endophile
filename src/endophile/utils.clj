(ns endophile.utils
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html])
  (:import [java.io StringWriter StringReader]
           [org.w3c.tidy Tidy]))

(defn a-attrs [mapping]
  (into {} (filter (fn [[k v]] (or (not (str/blank? v)) (= k :href))) mapping)))

(defn tidy [untidy]
  (let [w (StringWriter.)]
    (doto (Tidy.)
      (.setTabsize 4)
      (.setPrintBodyOnly true)
      (.setShowWarnings false)
      (.setQuiet true)
      ;; this oddity prevents Tidy from returning a blank string when given
      ;; a string containing only a comment.
      (.parse (StringReader. (str "<p></p>" untidy "<p></p>")) w))
    (-> (.toString w)
        (str/replace  "\r\n" "\n")
        (str/replace "\r" "\n")
        (str/replace #"\n+" "\n"))))

(defn xml-str
 "escapes < > and &, while leaving html entities unchanged.
 e.g. (xml-str \"&lt;p&gt tom & dick &amp; harry &lt;/p&gt; \" )
     ;; => \"&lt;p&gt tom &amp; dick &amp; harry &lt;/p&gt;\" "
 [x]
  (-> x str (.replace "<" "&lt;") (.replace ">" "&gt;")
      html/html-snippet first ;; reduce char entities to their values
      (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))

(defn verbatim-xml-str
 "escapes < > and &.
 e.g. (xml-str \"&lt;p&gt tom & dick &amp; harry &lt;/p&gt; \" )
     ;; => \"&amp;lt;p&amp;gt tom &amp; dick &amp; harry &amplt;/p&ampgt;\" "
 [x]
  (-> x str (.replace "&" "&amp;") (.replace "<" "&lt;") (.replace ">" "&gt;")))
