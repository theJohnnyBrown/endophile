(ns endophile.core
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
            TableColumnNode TableColumnNode$Alignment TableCaptionNode]
           [org.pegdown PegDownProcessor Extensions]))

;; See https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/Extensions.java
;; for descriptions
(def extensions
  {:smarts               Extensions/SMARTS
   :quotes               Extensions/QUOTES
   :smartypants          Extensions/SMARTYPANTS
   :abbreviations        Extensions/ABBREVIATIONS
   :hardwraps            Extensions/HARDWRAPS
   :autolinks            Extensions/AUTOLINKS
   :tables               Extensions/TABLES
   :definitions          Extensions/DEFINITIONS
   :fenced-code-blocks   Extensions/FENCED_CODE_BLOCKS
   :wikilinks            Extensions/WIKILINKS
   :strikethrough        Extensions/STRIKETHROUGH
   :anchorlinks          Extensions/ANCHORLINKS
   :all                  Extensions/ALL
   :suppress-html-blocks Extensions/SUPPRESS_HTML_BLOCKS
   :supress-all-html     Extensions/SUPPRESS_ALL_HTML
   :atxheaderspace       Extensions/ATXHEADERSPACE
   :forcelistitempara    Extensions/FORCELISTITEMPARA
   :relaxedhrules        Extensions/RELAXEDHRULES
   :tasklistitems        Extensions/TASKLISTITEMS
   :extanchorlinks       Extensions/EXTANCHORLINKS
   :all-optionals        Extensions/ALL_OPTIONALS
   :all-with-optionals   Extensions/ALL_WITH_OPTIONALS})

(defn- bit-or'
  "Bit-or which works if only one argument is given."
  [& xs]
  (if (seq (rest xs))
    (apply bit-or xs)
    (first xs)))

(defn extensions-map->int [opts]
  (->> opts
       (merge {:autolinks true
               :strikethrough true
               :fenced-code-blocks true})
       (filter val)
       keys
       (map extensions)
       (apply bit-or')
       int))

(defn mp
  "Parses given markdown.

   Second (optional) parameter is options map.

   Third (optional) parameter is parsing timeout in milliseconds (pegdown default: 2000 ms).

   Available options:
   - :extensions - Map of extensions to enable or disable. Check
     endophile.core/extensions for available extensions."
  ([md] (mp md {}))
  ([md opts]
   (mp md opts PegDownProcessor/DEFAULT_MAX_PARSING_TIME))
  ([md opts ^Long timeout]
   (.parseMarkdown
     (PegDownProcessor. (extensions-map->int (:extensions opts)) timeout)
     (char-array md))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods return clojure representation of HTML nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol AstToClj
  (to-clj-with-context [node context]))

(defn to-clj [node]
  (to-clj-with-context node {}))

(defn clj-contents [node context]
  (doall (flatten (map #(to-clj-with-context % context)
                       (seq (.getChildren node))))))

(extend-type SuperNode AstToClj
  (to-clj-with-context [node context]
    (clj-contents node context)))

(extend-type RootNode AstToClj
  (to-clj-with-context [node context]
    (clj-contents node (add-references context
                                       clj-contents
                                       (.getReferences node)) )))

(extend-type BulletListNode AstToClj
  (to-clj-with-context [node context]
    {:tag :ul
     :content (clj-contents node context)}))

(extend-type ListItemNode AstToClj
  (to-clj-with-context [node context]
    {:tag :li :content (clj-contents node context)}))

(extend-type TextNode AstToClj
  ;; html-snippet converts entities back into text
  (to-clj-with-context [node _] (first (html/html-snippet (.getText node)))))

(extend-type AutoLinkNode AstToClj
  (to-clj-with-context [node _]
    {:tag :a
     :attrs (a-attrs {:href (.getText node)})
     :content (list (.getText node))}))

(extend-type BlockQuoteNode AstToClj
  (to-clj-with-context [node context]
    {:tag :blockquote
     :content (clj-contents node context)}))

(extend-type CodeNode AstToClj
  (to-clj-with-context [node _]
    {:tag :code
     :content (list (.getText node))}))

(extend-type ExpImageNode AstToClj
  (to-clj-with-context [node context]
    {:tag :img
     :attrs (a-attrs
             {:src (.url node)
              :title (.title node)
              :alt (str/join (clj-contents node context))})}))

(extend-type ExpLinkNode AstToClj
  (to-clj-with-context [node context]
    {:tag :a
     :attrs (a-attrs {:href (.url node) :title (.title node)})
     :content (clj-contents node context)}))

(extend-type HeaderNode AstToClj
  (to-clj-with-context [node context]
    {:tag (keyword (str "h" (.getLevel node)))
     :content (clj-contents node context)}))


(extend-type HtmlBlockNode AstToClj
  (to-clj-with-context [node _]
    (html/html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToClj
  (to-clj-with-context [node _] (html/html-snippet (.getText node))))


(extend-type MailLinkNode AstToClj
  (to-clj-with-context [node _] {:tag :a
                    :attrs (a-attrs {:href (str "mailto:" (.getText node))})
                    :content (list (.getText node))}))

(extend-type OrderedListNode AstToClj
  (to-clj-with-context [node context]
    {:tag :ol
     :content (clj-contents node context)}))

(extend-type ParaNode AstToClj
  (to-clj-with-context [node context]
    {:tag :p
     :content (clj-contents node context)}))

(def qts
  {QuotedNode$Type/DoubleAngle [\u00AB \u00BB]
   QuotedNode$Type/Double [\u201C \u201D]
   QuotedNode$Type/Single [\u2018 \u2019]})

(extend-type QuotedNode AstToClj
  (to-clj-with-context [node context]
    {:tag :p
     :content (flatten
               (let [q (qts (.getType node))]
                 (list (q 0) (clj-contents node context) (q 1))))}))

(def simple-nodes
  {SimpleNode$Type/Apostrophe \'
   SimpleNode$Type/Ellipsis \u2026
   SimpleNode$Type/Emdash \u2014
   SimpleNode$Type/Endash \u2013
   SimpleNode$Type/HRule {:tag :hr}
   SimpleNode$Type/Linebreak {:tag :br}
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToClj
  (to-clj-with-context [node _] (simple-nodes (.getType node))))


(extend-type SpecialTextNode AstToClj
  (to-clj-with-context [node _] (.getText node)))

(extend-type StrongEmphSuperNode AstToClj
  (to-clj-with-context [node context]
    {:tag (if (.isStrong node) :strong :em)
     :content (clj-contents node context)}))

(extend-type StrikeNode AstToClj
  (to-clj-with-context [node context]
    {:tag :del
     :content (clj-contents node context)}))

(extend-type AnchorLinkNode AstToClj
  (to-clj-with-context [node _]
    {:tag :a
     :attrs {:name (.getName node)
             :href (str "#" (.getName node))}
     :content (list (.getText node))}))

(extend-type VerbatimNode AstToClj
  (to-clj-with-context [node _]
    {:tag :pre
     :content (list (merge {:tag :code
                            :content (list (.getText node))}
                           (when-let [c (.getType node)]
                             (if-not (or (str/blank? c)
                                         (nil? c))
                               {:attrs {:class c}}))))}))

(extend-type RefLinkNode AstToClj
  (to-clj-with-context [node context]
    (let [contents (clj-contents node context)
          key (if-let [nd (.referenceKey node)]
                (first (to-clj-with-context nd context))
                (apply str contents))]
      (if-let [ref ((:references context) key)]
        {:tag :a :attrs (a-attrs {:href (.getUrl ref) :title (.getTitle ref)})
         :content contents}
        (cons "[" (concat contents
                          (if (.separatorSpace node)
                            [(str "]"
                                  (.separatorSpace node)
                                  "[" (.referenceKey node) "]")]
                            ["]"])))))))

(extend-type ReferenceNode AstToClj
  (to-clj-with-context [node _]
    ""))

(extend-type TableNode AstToClj
  (to-clj-with-context [node context]
    {:tag :table
     :content (clj-contents node
                            (assoc context :table-columns
                                   (.getColumns node)))}))

(extend-type TableHeaderNode AstToClj
  (to-clj-with-context [node context]
    {:tag :thead
     :content (clj-contents node (assoc context :in-header true))}))

(extend-type TableBodyNode AstToClj
  (to-clj-with-context [node context]
    {:tag :tbody
     :content (clj-contents node (assoc context :in-header false))}))

(extend-type TableRowNode AstToClj
  (to-clj-with-context [node context]
    {:tag :tr
     :content (table-row-contents flatten
                                  to-clj-with-context
                                  node context)}))

(extend-type TableCellNode AstToClj
  (to-clj-with-context [node context]
    (let [alignment (.getAlignment (:column context))
          attrs (merge {}
                       (when (> (.getColSpan node) 1)
                         {:colspan (.getColSpan node)})
                       (when (not= alignment
                                   TableColumnNode$Alignment/None)
                         {:alignment (column-alignment alignment)}))]
      (merge {:tag (if (:in-header context) :th :td)
              :content (clj-contents node context)}
             (when (not (empty? attrs))
               {:attrs attrs})))))

(extend-type TableCaptionNode AstToClj
  (to-clj-with-context [node context]
    {:tag :caption
     :content (clj-contents node context)}))

(defn html-string [clj-md]
  (str/join (html/emit* clj-md)))

(defn to-html [parsed]
  (html-string
   {:tag :html
      :content
      (list
       {:tag :head :content
        (list {:tag :meta :attrs {:charset "utf-8"}})}
       {:tag :body
        :content (to-clj parsed)})}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [file]
  (println (to-html (mp (slurp file)))))
