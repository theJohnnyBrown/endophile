(ns endophile.utils
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [clojure.walk :as walk]))

(defn a-attrs [mapping]
  (into {} (filter (fn [[k v]] (or (not (str/blank? v)) (= k :href))) mapping)))

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

;;
;; HTML Tidy
;;

(defn- normalize-comment [text]
  (-> text
      ; Tagsoup (enlive) reads "--" as "- -"
      (str/replace #"- -" "--")
      ; Hiccup writes "--" as "=="
      (str/replace #"==" "--")))

(defn tidy [enlive-html]
  (walk/postwalk
    (fn [x]
      (cond
        (= :comment (:type x)) (assoc x :data (normalize-comment (:data x)))
        (string? x) (-> x
                        str/trim
                        (str/replace #"\n" " "))
        (seq? x) (into (empty x) (remove #{""} x))
        :else x))
    enlive-html))
