(ns mantl.core
  (:import (org.antlr.v4.runtime ANTLRInputStream
                                 CommonTokenStream
                                 ListTokenSource
                                 TokenFactory
                                 BufferedTokenStream
                                 Token)
           (org.antlr.v4.runtime.misc Interval)
           (org.antlr.v4.runtime.tree RuleNode
                                      ErrorNode
                                      TerminalNode)))

;This is an immutable equivalent of an ANTLR4 token; all it is missing is the
; InputStream (which by it's nature is mutable) and the
; TokenSource (which is often mutable). These immutable fields still (more-than)
; represent the useful information in a token.
(defrecord token
  [type text channel start-index stop-index line position-in-line token-index]
  Token
  (getChannel [this] (or channel Token/DEFAULT_CHANNEL))
  (getCharPositionInLine [this] (or position-in-line 0))
  (getInputStream [this] nil)
  (getLine [this] (or line 0))
  (getStartIndex [this] (or start-index -1))
  (getStopIndex [this] (or stop-index -1))
  (getText [this] text)
  (getTokenIndex [this] token-index)
  (getTokenSource [this] nil)
  (getType [this] type))

(defn reify-token
  "Creates an immutable Clojure record from an ANTLR Token."
  [t]
  (->token (.getType t) 
           (.getText t) 
           (.getChannel t) 
           (.getStartIndex t)
           (.getStopIndex t) 
           (.getLine t) 
           (.getCharPositionInLine t) 
           (.getTokenIndex t)))


(def token-factory
  "An ANTLR4 TokenFactory that returns Tokens that are immutable Clojure records.
  Returned Tokens have the immutable attributes of a CommonToken.
  Type is a unique integer, dependent on the Lexer identifying the Lexer rule.
  Text is the String the token was processed from.
  Channel is the token channel specified in the Lexer.
  Start is the position in the input String corresponding to the first character of the token.
  Stop is the position in the input String corresponding to the last character of the token.
  Line is the number of newlines in the input String preceding the first character of the token.
  Position-in-line is the number of characters preceeding the first character of the token to the most recent newline."
  
  (reify TokenFactory
    (create [this source type text channel start stop line position-in-line]
      (let [char-source (.b source)
            text-interval (if (<= 0 start stop) (Interval. start stop))
            ;If the text isn't passed in directly, extract it from the source
            text (or text (and char-source text-interval (.getText char-source text-interval)))]
      ;token-index -1 is the convention from CommonTokenFactory
      (->token type text channel start stop line position-in-line -1)))
    (create [this type text]
      (map->token {:type type :text text}))))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Text Mangling

(defn- unproper-name
  "Takes a string and converts the first letter to lower case."
  [s]
  {:pre [(string? s)]
   :post [(string? %) (or (not (Character/isLetter (first %)))
                          (Character/isLowerCase (first %)))]}
  (apply str
    (cons (Character/toLowerCase (first s)) (rest s))))

(defn- name-from-ANTLR-context
  "Gets the name of a rule extracting it from the generated class
  name. This is used over the array tokenNames
  in the Parser because this does not contain labelled rules in the grammar
  (i.e. rules names with #...)."
    [x]
    {:pre [(string? x)
           (.endsWith x "Context")]}
    (let  [name-len (- (.length x) 7)]
    ;Make the name lower case
    (unproper-name (.substring x 0 name-len))))

(defn- name-of-ANTLR-rule
  [r]
  (name-from-ANTLR-context (.getSimpleName (class r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn rule->map [r]
  {:type 'rule
   :name (name-of-ANTLR-rule r)
   :src-line (.getLine (.getStop r))})

(defn rule? [r]
  (and (map? r) (= (:type r) 'rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn error->map 
  "Handle an ErrorNode. This is currently inadequate."
  [e]
  {:type 'error
   :exception (.getText e)})

(defn unwrapper [rule-fn terminal-fn error-fn e]
  (cond
    (instance? ErrorNode e) (error-fn (.getSymbol e))
    (instance? TerminalNode e) (terminal-fn (.getSymbol e))
    (instance? RuleNode e) (rule-fn e)
    :else (throw (Exception. (str "Error: unknown parseTree in build-tree at: " e )))))

(defn- tree-map [f branch? children make-branch node]
  (if (branch? node)
    (make-branch (f node) (map #(tree-map f branch? children make-branch %) (children node)))
    (f node)))

(defn total-unwrapper [u x] (tree-map u
                                    #(instance? RuleNode %)
                                    #(.children %)
                                    #(assoc %1 :children %2)
                                    x))


(defn- count-newlines [s]
  "Counts the number of occurances of newlines in a String"
  {:pre [(string? s)]}
  (count (filter #(= % \newline) s)))


(defn- lexerClassname [grammar]
  (symbol (str grammar "Lexer")))
(defn- parserClassname [grammar]
  (symbol (str grammar "Parser")))

(defmacro importLexer 
  "Explicitly imports the ANTLR Lexer grammar from package. The Lexer class must be on the classpath."
  ([grammar package]
   (if package
     `(import (~package ~(lexerClassname grammar)))
     `(import ~(lexerClassname grammar)))))

(defmacro importParser 
  "Explicitly imports the ANTLR Parser grammar from package. The Parser class must be on the classpath."
  ([grammar package]
   (if package
     `(import (~package ~(parserClassname grammar))))
     `(import ~(parserClassname grammar))))

(defmacro ANTLR-lexer
  "Returns a function taking an ANTLR CharStream into the ANTLR Lexer grammar in package.
  The generated ANTLR class grammar in package must be on the class path.
  Prefer using lexer-parser or lexer if possible."
  ([grammar package]
   (let [arg (gensym)]
     `(do
        (importLexer ~grammar ~package)
        (fn [~arg]
          (new ~(lexerClassname grammar) ~arg)))))
  ([grammar] `(ANTLR-lexer ~grammar nil)))

(defmacro lexer 
  "Returns a lexer mapping a string into an array-list of Tokens defined by Lexer grammar in package.
  The generated class grammar in package must be on the class path.
  The returned Tokens are immutable Clojure records."
  ([grammar package]
   (let [arg (gensym)]
     `(fn [~arg]
        (-> ~arg
             ANTLRInputStream.
             ((ANTLR-lexer ~grammar ~package))
             (doto (.setTokenFactory token-factory))
             (doto .removeErrorListeners)
             .getAllTokens
             ))))
  ([grammar] `(lexer ~grammar nil)))

(defmacro ANTLR-parser
  "Returns a function taking an ANTLR TokenStream into the ANTLR parser in package.
  The generated ANTLR class grammar in package must be on the class path.
  Prefer using lexer-parser or parser if possible."
  ([grammar package]
   (let [token-stream (gensym)]
     `(do
        (importParser ~grammar ~package)
        (fn [~token-stream]
          (new ~(parserClassname grammar) ~token-stream)))))
  ([grammar] `(ANTLR-parser ~grammar nil)))

(defmacro parser
  "Returns a parser mapping a seqable of Tokens into a syntax-tree defined by grammar in package with start rule.
  The generated ANTLR class grammar in package must be on the class path.
  The syntax-tree is a nested array of Clojure maps.
  The rule nodes have the structure {:src-line :name :type :children}."
  ([rule grammar package]
   (let [source (gensym)]
     `(fn [~source]
        (total-unwrapper (partial unwrapper rule->map identity error->map)
                         (->
                           ~source
                           ListTokenSource.
                           BufferedTokenStream.
                           ((ANTLR-parser ~grammar ~package))
                           ;Don't print to std out
                           (doto .removeErrorListeners)
                           (doto (.setTokenFactory token-factory))
                           (. ~rule))))))
  ([rule grammar] `(parser ~rule ~grammar nil)))

(defmacro lexer-parser
  "Returns a lexer-parser mapping a String into a syntax-tree defined by grammar in package with start rule.
  The generated ANTLR class grammar in package must be on the class path.
  The syntax-tree is a nested array of Clojure maps.
  The rule nodes have the structure {:src-line :name :type :children}."
  ([rule grammar package]
   (let [arg (gensym)]
     `(fn [~arg]
        (total-unwrapper (partial unwrapper rule->map reify-token error->map)
                         (->
                           ~arg
                           ANTLRInputStream.
                           ((ANTLR-lexer ~grammar ~package))
                           (doto .removeErrorListeners)
                           CommonTokenStream.
                           ((ANTLR-parser ~grammar ~package))
                           ;Don't print errors to STDOUT
                           (doto .removeErrorListeners)
                           (. ~rule))))))
  ([rule grammar] `(lexer-parser ~rule ~grammar nil)))


