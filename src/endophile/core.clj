(ns endophile.core
  (:require [net.cgrand.enlive-html :as html])
  (:import [org.pegdown.ast
            RootNode BulletListNode ListItemNode SuperNode TextNode
            AutoLinkNode BlockQuoteNode CodeNode TextNode EmphNode ExpImageNode
            ExpLinkNode HeaderNode HtmlBlockNode InlineHtmlNode MailLinkNode
            OrderedListNode ParaNode QuotedNode QuotedNode$Type SimpleNode
            SimpleNode$Type SpecialTextNode StrongNode VerbatimNode]
           [org.pegdown PegDownProcessor Extensions]))

 (defn mp [md] (.parseMarkdown
                (PegDownProcessor. (int
                                    (bit-or
                                     Extensions/AUTOLINKS
                                     Extensions/FENCED_CODE_BLOCKS)))
                (char-array md)))

;; rendering
;; TODO references, abbreviations, tables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods return clojure representation of HTML nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol AstToClj
  (to-clj [node]))

(defn clj-contents [node]
  (flatten (map to-clj (seq (.getChildren node)))))

(extend-type SuperNode AstToClj
  (to-clj [node] (clj-contents node)))

(extend-type RootNode AstToClj
  (to-clj [node] (clj-contents node)))

(extend-type BulletListNode AstToClj
  (to-clj [node] {:tag :ul
                  :content (clj-contents node)}))

(extend-type ListItemNode AstToClj
  (to-clj [node] {:tag :li :content (flatten
                                     (map to-clj (seq (.getChildren node))))}))

(extend-type TextNode AstToClj
  (to-clj [node] (.getText node)))

(extend-type AutoLinkNode AstToClj
  (to-clj [node] {:tag :a
                  :attrs {:href (.getText node)}
                  :content (.getText node)}))

(extend-type BlockQuoteNode AstToClj
  (to-clj [node] {:tag :blockquote
                  :content (clj-contents node)}))

(extend-type CodeNode AstToClj
  (to-clj [node] {:tag :code
                  :content (.getText node)}))

(extend-type EmphNode AstToClj
  (to-clj [node] {:tag :em
                  :content (clj-contents node)}))

(extend-type ExpImageNode AstToClj
  (to-clj [node] {:tag :img
                  :attrs {:src (.url node) :title (.title node)
                          :alt (clj-contents node)}}))

(extend-type ExpLinkNode AstToClj
  (to-clj [node] {:tag :a
                  :attrs {:href (.url node) :title (.title node)}
                  :content (clj-contents node)}))

(extend-type HeaderNode AstToClj
  (to-clj [node] {:tag (keyword (str "h" (.getLevel node)))
                  :content (clj-contents node)}))


(extend-type HtmlBlockNode AstToClj
  (to-clj [node] (html/html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToClj
  (to-clj [node] (html/html-snippet (.getText node))))


(extend-type MailLinkNode AstToClj
  (to-clj [node] {:tag :a
                  :attrs {:href (str "mailto:" (.getText node))}
                  :content (clj-contents node)}))

(extend-type OrderedListNode AstToClj
  (to-clj [node] {:tag :ol
                  :content (clj-contents node)}))

(extend-type ParaNode AstToClj
  (to-clj [node] {:tag :p
                  :content (clj-contents node)}))

(def qts
  {QuotedNode$Type/DoubleAngle [\u00AB \u00BB]
   QuotedNode$Type/Double [\u201C \u201D]
   QuotedNode$Type/Single [\u2018 \u2019]})

(extend-type QuotedNode AstToClj
  (to-clj [node] {:tag :p
                  :content (flatten
                            (let [q (qts (.getType node))]
                              (list (q 0) (clj-contents node) (q 1))))}))

(def simple-nodes
  {SimpleNode$Type/Apostrophe \'
   SimpleNode$Type/Ellipsis \u2026
   SimpleNode$Type/Emdash \u2014
   SimpleNode$Type/Endash \u2013
   SimpleNode$Type/HRule {:tag :hr}
   SimpleNode$Type/Linebreak {:tag :br}
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToClj
  (to-clj [node] (simple-nodes (.getType node))))


(extend-type SpecialTextNode AstToClj
  (to-clj [node] (.getText node)))

(extend-type StrongNode AstToClj
  (to-clj [node] {:tag :strong
                  :content (clj-contents node)}))

(extend-type VerbatimNode AstToClj
  (to-clj [node]
    {:tag :pre
     :content (list {:tag :code :content (.getText node)})}))

(extend-type StrongNode AstToClj
  (to-clj [node] {:tag :strong
                  :content (clj-contents node)}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [file]
  (println (apply str
            (html/emit* {:tag :html
                         :content
                         (list
                          {:tag :head :content
                           (list {:tag :meta :attrs {:charset "utf-8"}})}
                          {:tag :body
                           :content (to-clj (mp (slurp file)))})}))))
