(defsystem #:dispatch-experiment
  :depends-on (:alexandria
               :let-plus
               :utilities.print-tree)
  :components ((:module       "src"
                :serial     t
                :components ((:file "package")

                             (:file "decision-tree")

                             ;;
                             (:file "typep-decision-procedure")
                             (:file "typep-generic-function")

                             ;;
                             (:file "interval-decision-procedure")

                             ;; Layout generic function
                             (:file "call-history")
                             (:file "layout-generic-function")

                             ;; Benchmark stuff
                             (:file "benchmark-generic")
                             (:file "benchmark-only-standard-objects")))))
