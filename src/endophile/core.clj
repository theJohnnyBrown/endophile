(ns endophile.core
  (:require [net.cgrand.enlive-html :as html])
  (:use clojure.pprint)
  (:import [org.pegdown.ast
            RootNode BulletListNode ListItemNode SuperNode TextNode RefLinkNode
            AutoLinkNode BlockQuoteNode CodeNode TextNode EmphNode ExpImageNode
            ExpLinkNode HeaderNode HtmlBlockNode InlineHtmlNode MailLinkNode
            OrderedListNode ParaNode QuotedNode QuotedNode$Type SimpleNode
            SimpleNode$Type SpecialTextNode StrongNode VerbatimNode
            ReferenceNode]
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
(declare ^:dynamic *references*)

(defprotocol AstToClj
  (to-clj [node]))

(defn clj-contents [node]
  (doall (flatten (map to-clj (seq (.getChildren node))))))

(extend-type SuperNode AstToClj
  (to-clj [node] (clj-contents node)))

(extend-type RootNode AstToClj
  (to-clj [node]
    (binding [*references*
              (into {}
                    (for [ref (.getReferences node)]
                      [(first (clj-contents ref)) ref]))]
     (clj-contents node))))

(extend-type BulletListNode AstToClj
  (to-clj [node] {:tag :ul
                  :content (clj-contents node)}))

(extend-type ListItemNode AstToClj
  (to-clj [node] {:tag :li :content (clj-contents node)}))

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

(extend-type RefLinkNode AstToClj
  (to-clj [node]
    (let [contents (clj-contents node)
          key (if-let [nd (.referenceKey node)]
                (first (to-clj nd)) (apply str contents))]
     (if-let [ref (*references* key)]
       {:tag :a :attrs (into {}
                         (filter #(% 1)
                                 [[:href (.getUrl ref)]
                                  [:title (.getTitle ref)]]))
        :content contents}
       (cons "[" (concat contents
                         (if (.separatorSpace node)
                             [(str "]"
                                   (.separatorSpace node)
                                   "[" (.referenceKey node) "]")]
                             ["]"])))))))

(extend-type ReferenceNode AstToClj
  (to-clj [node]
    ""))

(defn to-html [parsed]
  (apply str
    (html/emit* {:tag :html
                 :content
                 (list
                  {:tag :head :content
                   (list {:tag :meta :attrs {:charset "utf-8"}})}
                  {:tag :body
                   :content (to-clj parsed)})})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [file]
  (println (to-html (mp (slurp file)))))

;; (defn crn [nd] (.getChildren nd))
;; (defn txt [nd ] (.getText nd))
;; (defn bound-clj [node]
;;       (binding [*references* refs]
;;         (to-clj node)))
(def parsed (mp (slurp "test/resources/Links, reference style.text")))
;; (-> parsed
;;     crn
;;     (nth 4)
;;     crn
;;     first
;;     crn
;;     (nth 1)
;;     bound-clj
;;     ;; crn
;;     ;; first
;;     ;; crn
;;     ;; (nth 1)
;;     ;; crn




;;     )
