(defproject ast-eq "0.1.1"
  :description "Verifies equality between two data structures with unique elements (eg: gensym)"
  :url "https://github.com/Pancia/ast-eq"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [machine "1.0.0"]
                 [navis/untangled-spec "0.3.5" :scope "test"]]
  :test-refresh {:changes-only true
                 :with-repl true
                 :report untangled-spec.reporters.terminal/untangled-report}
  )
