(defproject org.clojars.eureton/cljmd "0.1.0"
  :description "Clojure implementation of CommonMark"
  :url "https://github.com/eureton/cljmd"
  :license {:name "MIT"
            :url "https://github.com/eureton/cljmd/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/core.incubator "0.1.4"]
                 [org.clojars.eureton/squirrel "0.1.0"]
                 [org.flatland/useful "0.11.6"]
                 [cheshire "5.10.0"]]
  :repl-options {:init-ns cljmd.core})
