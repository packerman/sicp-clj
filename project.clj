(defproject sicp-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.namespace "0.2.11"]]
  :repl-options {
                 :init (do
                         (require '[clojure.test :refer [run-tests]])
                         (require '[clojure.tools.namespace.repl :refer [refresh]]))})
