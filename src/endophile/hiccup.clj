(ns endophile.hiccup
  (:require [net.cgrand.enlive-html :as html])
  (:import [org.pegdown.ast
            RootNode BulletListNode ListItemNode SuperNode TextNode
            AutoLinkNode BlockQuoteNode CodeNode TextNode EmphNode ExpImageNode
            ExpLinkNode HeaderNode HtmlBlockNode InlineHtmlNode MailLinkNode
            OrderedListNode ParaNode QuotedNode QuotedNode$Type SimpleNode
            SimpleNode$Type SpecialTextNode StrongNode VerbatimNode]
           [org.pegdown PegDownProcessor Extensions]))

(defn- sequential-but-not-vector? [s]
  (and (sequential? s) (not (vector? s))))

(defn- flatten*
  [x]
  (filter (complement sequential-but-not-vector?)
          (rest (tree-seq sequential-but-not-vector? seq x))))

(defprotocol AstToHiccup
  (to-hiccup [node]))

(defn clj-contents [node]
  (flatten* (map to-hiccup (seq (.getChildren node)))))

(extend-type SuperNode AstToHiccup
  (to-hiccup [node] (clj-contents node)))

(extend-type RootNode AstToHiccup
  (to-hiccup [node] (clj-contents node)))

(extend-type BulletListNode AstToHiccup
  (to-hiccup [node] (vec (cons :ul (clj-contents node)))))

(extend-type ListItemNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :li (flatten* (map to-hiccup (seq (.getChildren node))))))))

(extend-type TextNode AstToHiccup
  (to-hiccup [node] (.getText node)))

(extend-type AutoLinkNode AstToHiccup
  (to-hiccup [node] 
    [:a {:href (.getText node)} (.getText node)]))

(extend-type BlockQuoteNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :blockquote (clj-contents node)))))

(extend-type CodeNode AstToHiccup
  (to-hiccup [node] 
    [:code (.getText node)]))

(extend-type EmphNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :em (clj-contents node)))))

(extend-type ExpImageNode AstToHiccup
  (to-hiccup [node] 
    [:img {:src (.url node) :title (.title node) :alt (clj-contents node)}]))

(extend-type ExpLinkNode AstToHiccup
  (to-hiccup [node] 
    (vec
     (concat
      [:a {:href (.url node) :title (.title node)}]
      (clj-contents node)))))

(extend-type HeaderNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons (keyword (str "h" (.getLevel node))) 
               (clj-contents node)))))

(extend-type HtmlBlockNode AstToHiccup
  (to-hiccup [node] 
    (html/html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToHiccup
  (to-hiccup [node] 
    (html/html-snippet (.getText node))))

(extend-type MailLinkNode AstToHiccup
  (to-hiccup [node] 
    (vec (concat [:a {:href (str "mailto:" (.getText node))}]
                 (clj-contents node)))))

(extend-type OrderedListNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :ol (clj-contents node)))))

(extend-type ParaNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :p (clj-contents node)))))

(def qts
  {QuotedNode$Type/DoubleAngle [\u00AB \u00BB]
   QuotedNode$Type/Double [\u201C \u201D]
   QuotedNode$Type/Single [\u2018 \u2019]})

(extend-type QuotedNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :p (flatten*
                   (let [q (qts (.getType node))]
                     (list (q 0) (clj-contents node) (q 1))))))))

(def simple-nodes
  {SimpleNode$Type/Apostrophe \'
   SimpleNode$Type/Ellipsis \u2026
   SimpleNode$Type/Emdash \u2014
   SimpleNode$Type/Endash \u2013
   SimpleNode$Type/HRule {:tag :hr}
   SimpleNode$Type/Linebreak {:tag :br}
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToHiccup
  (to-hiccup [node] (simple-nodes (.getType node))))

(extend-type SpecialTextNode AstToHiccup
  (to-hiccup [node] (.getText node)))

(extend-type StrongNode AstToHiccup
  (to-hiccup [node] 
    (vec (cons :strong (clj-contents node)))))

(extend-type VerbatimNode AstToHiccup
  (to-hiccup [node]
    (vec (cons :pre (list {:tag :code :content (.getText node)})))))

