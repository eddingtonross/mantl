(ns mantl.core
  (:import (org.antlr.v4.runtime ANTLRInputStream
                                 RuleContext
                                 CommonTokenStream
                                 ListTokenSource
                                 CommonToken
                                 Token)
           (org.antlr.v4.runtime.tree ParseTreeWalker
                                      RuleNode
                                      ErrorNode
                                      TerminalNode))
  (:require clojure.walk))

;;Unwrapping
(defn unwrap-token
  "Takes an ANTLR Token and returns a Clojure datatype representing that Token."
  [e]
  {:pre [(instance? Token e)]}
  (let [startIndex (.getStartIndex e)
        stopIndex (.getStopIndex e)]
    (merge
      {;:token-name (token-name e)
       :value (.getText e)
       :line (.getLine e)
       :position (.getCharPositionInLine e)
       :channel (.getChannel e)
       :type (.getType e)
       :index (.getTokenIndex e)
       ;Avoid stateful TokenSource and InputStream and hope non-breaking
       }
      ;.getStart/StopIndex return -1 if not implemented
      (if (>= startIndex 0)
        {:start-index startIndex})
      (if (>= stopIndex 0)
        {:stop-index stopIndex}))))

;The TokenSource and InputStream are kaput by now
;Invariant: (comp unwrap-token wrap-token unwrap-token) = unwrap-token (on its domain)
(defn wrap-token
  [t]
  (doto
    (CommonToken. (:type t) (:value t))
    (.setChannel (:channel t))
    (.setLine (:line t))
    (.setCharPositionInLine (:position t))
    (.setTokenIndex (:index t))
    (.setStartIndex (get t :start-index -1))
    (.setStopIndex (get t :stop-index -1))))


(defn- unproper-name
  "Takes a string and converts the first letter to lower case."
  [s]
  {:pre [(string? s)]
   :post [(string? %) (or (not (Character/isLetter (first %)))
                          (Character/isLowerCase (first %)))]}
  (apply str
    (cons (Character/toLowerCase (first s)) (rest s))))

(defn- count-newlines [s]
  "Counts the number of occurances of newlines in a String"
  {:pre [(string? s)]}
  (count (filter #(= % \newline) s)))


(defn- lexerClassname [grammar]
  (symbol (str grammar "Lexer")))
(defn- parserClassname [grammar]
  (symbol (str grammar "Parser")))

(defmacro importLexer 
  ([grammar package]
   (if package
     `(import (~package ~(lexerClassname grammar)))
     `(import ~(lexerClassname grammar)))))

(defmacro importParser 
  ([grammar package]
   (if package
     `(import (~package ~(parserClassname grammar))))
     `(import ~(parserClassname grammar))))

(defmacro lexer 
  ([grammar package]
   (let [arg (gensym)]
     `(do
        (importLexer ~grammar ~package)
        (fn [~arg]
          (->> ~arg
             ANTLRInputStream.
             (new ~(lexerClassname grammar))
             .getAllTokens
             (map unwrap-token)
             )))))
  ([grammar] `(lexer ~grammar nil)))

(defmacro parser
  ;Inefficient for multiple partial parsers: Could share structure
  ([rule grammar package]
   (let [source (gensym)]
     `(do
        (importParser ~grammar ~package)
        (fn [~source]
          (->
            (doto
              (->> ~source
                   (map wrap-token)
                   ListTokenSource.
                   CommonTokenStream.
                   (new ~(parserClassname grammar)))
              ;Don't print to std out
              .removeErrorListeners)
            (. ~rule))))))
  ([rule grammar] `(parser ~rule ~grammar nil)))


(defmacro lexer-parser
  ([grammar package]
   (let [arg (gensym)]
     `(do
        (importLexer ~grammar ~package)
        (importParser ~grammar ~package)
        (fn [~arg]
          (doto
            (->>
              ~arg
              ANTLRInputStream.
              (new ~(lexerClassname grammar))
              CommonTokenStream.
              (new ~(parserClassname grammar)))
            ;Don't print errors to STDOUT
            .removeErrorListeners)))))
  ([grammar] `(lexer-parser ~grammar nil)))


;(defn antlr-parse
;  "Returns the top level rule (program) of the ANTLR parse of the
;  Java program s."
;  [s]
;  (let [parser (doto
;                 (-> s
;                     ANTLRInputStream.
;                     JavaLexer.
;                     CommonTokenStream.
;                     JavaParser.)
;                 ;Deal with errors in our own way
;                 .removeErrorListeners)]
;    (.compilationUnit parser)))


