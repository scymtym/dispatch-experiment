(cl:in-package #:dispatch-experiment)

(defstruct (entry
             (:constructor
              %make-entry (arguments layout outcomes))
             (:predicate nil)
             (:copier nil))
  (arguments nil :type list                       :read-only t)
  (layout    nil :type (or null sb-kernel:layout) :read-only t)
  (outcomes  '() :type list                       :read-only t))

(defun mergable-into-entry? (object outcomes entry)
  (let+ ((layout (argument-information (list object)))
         ((&structure-r/o entry- (old-layout layout) (old-outcomes outcomes))
          entry))
    (and (equal outcomes old-outcomes)
         (eq layout old-layout))))

(defun argument-information (arguments)
  (let+ (((&flet layout (object)
            (sb-kernel:layout-of object)))
         (layouts  (remove-duplicates (map 'list #'layout arguments))))
    (first layouts)))

(defun make-entry (arguments outcomes)
  (multiple-value-call #'%make-entry
    arguments (argument-information arguments) outcomes))

(defun extend-call-history (argument outcomes call-history)
  (if-let ((existing (find-if (curry #'mergable-into-entry? argument outcomes)
                              call-history)))
    (let ((new (make-entry (list* argument (entry-arguments existing)) outcomes)))
      (substitute new existing call-history))
    (list* (make-entry (list argument) outcomes) call-history)))
