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
            ReferenceNode StrikeNode AnchorLinkNode TableNode
            TableHeaderNode TableBodyNode TableRowNode TableCellNode
            TableColumnNode TableColumnNode$Alignment TableCaptionNode]))

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

(defprotocol AstToHiccup
  (to-hiccup-with-context [node context]))

(defn to-hiccup [node]
  (to-hiccup-with-context node {}))

(defn clj-contents [node context]
  (doall (flatten* (map #(to-hiccup-with-context % context)
                        (seq (.getChildren node))))))

(extend-type SuperNode AstToHiccup
  (to-hiccup-with-context [node context] (clj-contents node context)))

(extend-type RootNode AstToHiccup
  (to-hiccup-with-context [node context]
    (clj-contents node
                  (add-references context
                                  clj-contents
                                  (.getReferences node)))))

(extend-type BulletListNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :ul (clj-contents node context)))))

(extend-type ListItemNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :li (flatten* (map #(to-hiccup-with-context % context)
                                  (seq (.getChildren node))))))))

(extend-type TextNode AstToHiccup
  (to-hiccup-with-context [node _] (xml-str (.getText node))))

(extend-type AutoLinkNode AstToHiccup
  (to-hiccup-with-context [node _]
    [:a {:href (.getText node)}
     (xml-str (.getText node))]))

(extend-type BlockQuoteNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :blockquote (clj-contents node context)))))

(extend-type CodeNode AstToHiccup
  (to-hiccup-with-context [node _]
    [:code (verbatim-xml-str (.getText node))]))

(extend-type ExpImageNode AstToHiccup
  (to-hiccup-with-context [node context]
    [:img {:src   (.url node)
           :title (.title node)
           :alt   (apply str (clj-contents node context))}]))

(extend-type ExpLinkNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec
     (concat
      [:a (a-attrs {:href (.url node) :title (.title node)})]
      (clj-contents node context)))))

(extend-type HeaderNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons (keyword (str "h" (.getLevel node)))
               (clj-contents node context)))))

(extend-type HtmlBlockNode AstToHiccup
  (to-hiccup-with-context [node _]
    (html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToHiccup
  (to-hiccup-with-context [node _]
    (html-snippet (.getText node))))

(extend-type MailLinkNode AstToHiccup
  (to-hiccup-with-context [node _]
    (vec (concat [:a {:href (str "mailto:" (.getText node))}]
                 (list (.getText node))))))

(extend-type OrderedListNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :ol (clj-contents node context)))))

(extend-type ParaNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :p (clj-contents node context)))))

(def qts
  {QuotedNode$Type/DoubleAngle [\u00AB \u00BB]
   QuotedNode$Type/Double [\u201C \u201D]
   QuotedNode$Type/Single [\u2018 \u2019]})

(extend-type QuotedNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :p (flatten*
                   (let [q (qts (.getType node))]
                     (list (q 0) (clj-contents node context) (q 1))))))))

(def simple-nodes
  {SimpleNode$Type/Apostrophe \'
   SimpleNode$Type/Ellipsis \u2026
   SimpleNode$Type/Emdash \u2014
   SimpleNode$Type/Endash \u2013
   SimpleNode$Type/HRule [:hr ]
   SimpleNode$Type/Linebreak [:br ]
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToHiccup
  (to-hiccup-with-context [node _] (simple-nodes (.getType node))))

(extend-type SpecialTextNode AstToHiccup
  (to-hiccup-with-context [node _] (xml-str (.getText node))))

(extend-type StrongEmphSuperNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons (if (.isStrong node) :strong :em)
               (clj-contents node context)))))

(extend-type StrikeNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :del (clj-contents node context)))))

(extend-type AnchorLinkNode AstToHiccup
  (to-hiccup-with-context [node _]
    (vector :a {:name (.getName node):href (str "#" (.getName node))}
            (xml-str (.getText node)))))

(extend-type VerbatimNode AstToHiccup
  (to-hiccup-with-context [node _]
    [:pre [:code
           (when-let [c (.getType node)]
             (if-not (or (str/blank? c)
                         (nil? c))
               {:class c}))
           (verbatim-xml-str (.getText node))]]))

(extend-type RefLinkNode AstToHiccup
  (to-hiccup-with-context [node context]
    (let [contents (clj-contents node context)
          key (if-let [nd (.referenceKey node)]
                (str/join (to-hiccup-with-context nd context))
                (str/join contents))]
     (if-let [ref ((:references context) key)]
       [:a (a-attrs {:href (.getUrl ref) :title (.getTitle ref)}) contents]
       (cons "[" (concat contents
                         (if (.separatorSpace node)
                             [(str "]"
                                   (.separatorSpace node)
                                   "[" (.referenceKey node) "]")]
                             ["]"])))))))

(extend-type ReferenceNode AstToHiccup
  (to-hiccup-with-context [_ _]
    nil))

(extend-type TableNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :table (clj-contents node
                                    (assoc context :table-columns
                                           (.getColumns node)))))))

(extend-type TableHeaderNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :thead (clj-contents node (assoc context :in-header true))))))

(extend-type TableBodyNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :tbody (clj-contents node (assoc context :in-header false))))))

(extend-type TableRowNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :tr (table-row-contents flatten*
                                       to-hiccup-with-context
                                       node context)))))

(extend-type TableCellNode AstToHiccup
  (to-hiccup-with-context [node context]
    (let [alignment (.getAlignment (:column context))
          attrs (merge {}
                       (when (> (.getColSpan node) 1)
                         {:colspan (.getColSpan node)})
                       (when (not= alignment
                                   TableColumnNode$Alignment/None)
                         {:alignment (column-alignment alignment)}))]
      (vec (remove nil?
                   (concat [(if (:in-header context) :th :td)
                            (when (not (empty? attrs)) attrs)]
                           (clj-contents node context)))))))

(extend-type TableCaptionNode AstToHiccup
  (to-hiccup-with-context [node context]
    (vec (cons :caption (clj-contents node context)))))
