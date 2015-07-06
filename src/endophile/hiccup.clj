(ns endophile.hiccup
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [endophile.utils :refer :all])
  (:import [org.pegdown.ast
            RootNode BulletListNode ListItemNode SuperNode TextNode RefLinkNode
            AutoLinkNode BlockQuoteNode CodeNode TextNode ExpImageNode
            ExpLinkNode HeaderNode HtmlBlockNode InlineHtmlNode MailLinkNode
            OrderedListNode ParaNode QuotedNode QuotedNode$Type SimpleNode
            SimpleNode$Type SpecialTextNode StrongEmphSuperNode VerbatimNode
            ReferenceNode StrikeNode AnchorLinkNode]))

(defn- sequential-but-not-vector? [s]
  (and (sequential? s) (not (vector? s))))

(defn- flatten*
  [x]
  (filter (complement sequential-but-not-vector?)
          (rest (tree-seq sequential-but-not-vector? seq x))))

(defn- clj2hiccup [clj-xml]
  (if-let [tag (:tag clj-xml)]
    [(keyword tag)
     (:attrs clj-xml)
     (clj2hiccup (:content clj-xml))]
    (cond
     (seq? clj-xml) (map clj2hiccup clj-xml)
     (string? clj-xml) (xml-str clj-xml)
     (= (:type clj-xml) :comment) (str "<!--" (:data clj-xml) "-->")
     :else nil)))

(defn- html-snippet [s]
  (clj2hiccup (html/html-snippet s)))

(declare ^:dynamic *references*)

(defprotocol AstToHiccup
  (to-hiccup [node]))

(defn clj-contents [node]
  (doall (flatten* (map to-hiccup (seq (.getChildren node))))))

(extend-type SuperNode AstToHiccup
  (to-hiccup [node] (clj-contents node)))

(extend-type RootNode AstToHiccup
  (to-hiccup [node]
    (binding [*references*
              (into {}
                    (for [ref (.getReferences node)]
                      [(first (clj-contents ref)) ref]))]
     (clj-contents node))))

(extend-type BulletListNode AstToHiccup
  (to-hiccup [node] (vec (cons :ul (clj-contents node)))))

(extend-type ListItemNode AstToHiccup
  (to-hiccup [node]
    (vec (cons :li (flatten* (map to-hiccup (seq (.getChildren node))))))))

(extend-type TextNode AstToHiccup
  (to-hiccup [node] (xml-str (.getText node))))

(extend-type AutoLinkNode AstToHiccup
  (to-hiccup [node]
    [:a {:href (.getText node)}
     (xml-str (.getText node))]))

(extend-type BlockQuoteNode AstToHiccup
  (to-hiccup [node]
    (vec (cons :blockquote (clj-contents node)))))

(extend-type CodeNode AstToHiccup
  (to-hiccup [node]
    [:code (verbatim-xml-str (.getText node))]))

(extend-type ExpImageNode AstToHiccup
  (to-hiccup [node]
    [:img {:src (.url node) :title (.title node) :alt (apply str (clj-contents node))}]))

(extend-type ExpLinkNode AstToHiccup
  (to-hiccup [node]
    (vec
     (concat
      [:a (a-attrs {:href (.url node) :title (.title node)})]
      (clj-contents node)))))

(extend-type HeaderNode AstToHiccup
  (to-hiccup [node]
    (vec (cons (keyword (str "h" (.getLevel node)))
               (clj-contents node)))))

(extend-type HtmlBlockNode AstToHiccup
  (to-hiccup [node]
    (html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToHiccup
  (to-hiccup [node]
    (html-snippet (.getText node))))

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
   SimpleNode$Type/HRule [:hr ]
   SimpleNode$Type/Linebreak [:br ]
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToHiccup
  (to-hiccup [node] (simple-nodes (.getType node))))

(extend-type SpecialTextNode AstToHiccup
  (to-hiccup [node] (xml-str (.getText node))))

(extend-type StrongEmphSuperNode AstToHiccup
  (to-hiccup [node]
    (vec (cons (if (.isStrong node) :strong :em) (clj-contents node)))))

(extend-type StrikeNode AstToHiccup
  (to-hiccup [node]
    (vec (cons :del (clj-contents node)))))

(extend-type AnchorLinkNode AstToHiccup
  (to-hiccup [node]
    (vector :a {:name (.getName node) :href (str "#" (.getName node))} (xml-str (.getText node)))))

(extend-type VerbatimNode AstToHiccup
  (to-hiccup [node]
    [:pre [:code
           (when-let [c (.getType node)]
             (if-not (or (str/blank? c)
                         (nil? c))
               {:class c}))
           (verbatim-xml-str (.getText node))]]))

(extend-type RefLinkNode AstToHiccup
  (to-hiccup [node]
    (let [contents (clj-contents node)
          key (if-let [nd (.referenceKey node)]
                (str/join (to-hiccup nd)) (str/join contents))]
     (if-let [ref (*references* key)]
       [:a (a-attrs {:href (.getUrl ref) :title (.getTitle ref)}) contents]
       (cons "[" (concat contents
                         (if (.separatorSpace node)
                             [(str "]"
                                   (.separatorSpace node)
                                   "[" (.referenceKey node) "]")]
                             ["]"])))))))

(extend-type ReferenceNode AstToHiccup
  (to-hiccup [node]
    nil))
