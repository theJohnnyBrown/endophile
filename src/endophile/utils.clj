(ns endophile.utils
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [clojure.walk :as walk])
  (:import [org.pegdown.ast
            TableColumnNode$Alignment]))

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

;;
;; Table columns
;;

(defn- assoc-column [columns context index]
  (assoc context :column (.get columns index)))

(defn- column-indices [node]
  (reductions + 0
              (map #(.getColSpan %)
                   (butlast (.getChildren node)))))

(defn- contexts-with-columns [node context]
  (let [columns (:table-columns context)]

    (map (partial assoc-column columns)
         (repeat context)
         (column-indices node))))

(defn table-row-contents [flatten-fn transform-with-context node context]
  (doall (flatten-fn (map transform-with-context
                          (seq (.getChildren node))
                          (contexts-with-columns node context)))))

(def column-alignment
  {TableColumnNode$Alignment/Left   "left"
   TableColumnNode$Alignment/Right  "right"
   TableColumnNode$Alignment/Center "center"})

;;
;; References
;;

(defn add-references [context contents-fn references]
  (assoc context :references
         (merge (:references context)
                (into {}
                      (for [ref references]
                        [(first (contents-fn ref context)) ref])))))
