(cl:in-package #:dispatch-experiment)

;;; Layout discrimination

(defun make-discriminating-function-form (entries miss-form &key assume-instance-p)
  (let* ((layout-addresses (map 'list (lambda+ ((&structure-r/o entry- layout))
                                        (sb-vm::get-lisp-obj-address layout))
                                entries))
         (interface        (make-instance 'interval-dispatch :number-var 'address))
         (tree             (make-decision-tree interface (map 'list (lambda (address)
                                                                      (cons address address))
                                                              layout-addresses))))
    (format t "Assuming argument is an instance: ~S~%" assume-instance-p)
    `(lambda (object)
       (declare (optimize (speed 3) (debug 0) (safety 0)))
       (let ((address (sb-vm::get-lisp-obj-address ,(if assume-instance-p
                                                        `(sb-kernel:%instance-layout object)
                                                        `(sb-kernel:layout-of object)))))
         ,(emit-decision-tree-code
           tree
           (lambda (candidates info)
             (declare (ignore info))
             (if candidates
                 `',(map 'list (lambda (address)
                                 (elt entries (position (car address) layout-addresses)))
                         candidates)
                 miss-form)))))))

(defun make-discriminating-function (entries miss-form &key assume-instance-p)
  (compile nil (make-discriminating-function-form
                entries miss-form :assume-instance-p assume-instance-p)))

;; Generic function

(defclass layout-gf (sb-mop:funcallable-standard-object)
  ((classes       :initarg  :classes
                  :accessor classes)
   (call-history  :accessor call-history
                  :initform '())
   (discriminator :accessor discriminator))
  (:metaclass sb-mop:funcallable-standard-class))

(defun cache-miss (generic-function object)
  (let ((outcomes (funcall (discriminator generic-function) object)))
    (setf (call-history generic-function)
          (extend-call-history object outcomes (call-history generic-function))))
  (sb-mop:set-funcallable-instance-function
   generic-function
   (make-discriminating-function
    (call-history generic-function)
    `(cache-miss ,generic-function object)
    :assume-instance-p (every (lambda+ ((&structure-r/o entry- outcomes))
                                (every (rcurry #'subtypep 'standard-object) outcomes))
                              (call-history generic-function))))
  (funcall generic-function object))

(defmethod shared-initialize :after ((instance   layout-gf)
                                     (slot-names t)
                                     &key
                                     classes)
  (setf (discriminator instance) (make-typep-discriminator classes))
  (sb-mop:set-funcallable-instance-function
   instance (lambda (object)
              (cache-miss instance object))))
