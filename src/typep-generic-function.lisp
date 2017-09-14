(cl:in-package #:dispatch-experiment)

;;; `typep'-based discriminating function

(defun make-typep-discriminator-form (types)
  (let* ((object-var 'object)
         (interface  (make-instance 'type-discrimination :object-var object-var))
         (tree       (make-decision-tree interface types)))
    `(lambda (,object-var)
       (declare (optimize (speed 3) (debug 0) (safety 0)))
       ,(emit-decision-tree-code
         tree (lambda (candidates info)
                (declare (ignore info))
                `',candidates)))))

(defun make-typep-discriminator (types)
  (compile nil (make-typep-discriminator-form types)))

;;; Generic function

(defclass typep-gf (sb-mop:funcallable-standard-object)
  ((discriminator :accessor discriminator))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod shared-initialize :after ((instance   typep-gf)
                                     (slot-names t)
                                     &key
                                     classes)
  (sb-mop:set-funcallable-instance-function
   instance (make-typep-discriminator classes)))
