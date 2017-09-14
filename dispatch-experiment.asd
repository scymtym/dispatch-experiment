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
                             (:file "layout-generic-function")

                             (:file "benchmark-generic")
                             (:file "benchmark-only-standard-objects")))))
