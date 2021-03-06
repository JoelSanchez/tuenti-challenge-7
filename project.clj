(defproject tuentichallenge "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [com.gearswithingears/async-sockets "0.1.0"]
                 [aysylu/loom "1.0.0"]
                 [clj-time "0.13.0"]
                 [com.taoensso/tufte "1.1.1"] 
                 [org.clojure/tools.trace "0.7.9"]]
  :repl-options {:init-ns user}
  :jvm-opts ["-Xmx6g" "-Xms6g" "-XX:-OmitStackTraceInFastThrow"] 
  :profiles {:dev {:source-paths ["dev"]}})
