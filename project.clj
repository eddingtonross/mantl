(defproject mantl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.antlr/antlr4-runtime "4.3"]]
  :plugins [[lein-antlr4 "0.1.1"]]
  :antlr-src-dir "."
  :antlr-dest-dir "gen-src"
  :antlr-options {:listener false}
  :java-source-paths ["gen-src"]
  :hooks [leiningen.antlr4]
  )