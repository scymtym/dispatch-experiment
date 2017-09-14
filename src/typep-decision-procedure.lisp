(cl:in-package #:dispatch-experiment)

(defclass type-discrimination ()
  ((object-var :initarg :object-var
               :reader  object-var))
  (:default-initargs
   :object-var (error "missing required initarg ~S" :object-var)))

(defun combined-implied-type (tests)
  `(and ,@(map 'list (lambda+ ((type polarity))
                       (if polarity
                           type
                           `(not ,type)))
               tests)))

(defmethod map-possible-tests ((interface      type-discrimination)
                               (function       function)
                               (previous-tests list)
                               (candidates     list))
  (map nil (lambda (type)
             (let+ ((test         (list type t))
                    (test/negated (list type nil))
                    ((&flet inhabited? (tests)
                       (not (subtypep (combined-implied-type tests) nil)))))
               (when (and (inhabited? (list* test         previous-tests))
                          (inhabited? (list* test/negated previous-tests)))
                 (funcall function test test/negated))))
       candidates))

(defmethod tests-info ((interface type-discrimination)
                       (tests     list))
  (list (combined-implied-type tests)))

(defmethod emit-test ((interface type-discrimination)
                      (test      t))
  (let+ (((type polarity) test)
         (base-form `(typep ,(object-var interface) ',type)))
    (if polarity
        base-form
        `(not ,base-form))))

(defmethod classify-candidate ((interface  type-discrimination)
                               (tests      list) ; TODO should accept pre-computed combined info here
                               (candidate  t))
  (let ((combined (combined-implied-type tests)))
    (cond
      ((subtypep combined candidate)
       t)
      ((subtypep combined `(not ,candidate))
       nil)
      (t
       :undetermined))))
