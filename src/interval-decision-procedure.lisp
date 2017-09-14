(cl:in-package #:dispatch-experiment)

(defclass interval-dispatch ()
  ((number-var :initarg :number-var
               :reader  number-var))
  (:default-initargs
   :number-var (error "missing required initarg ~S" :number-var)))

(defun combined-test-bounds (tests)
  (let+ ((lower most-negative-fixnum)
         (upper most-positive-fixnum)
         (holes '())
         ((&flet filter-holes (holes)
            (remove-if (lambda (hole)
                         (or (< hole lower) (< upper hole)))
                       holes)))
         ((&flet apply-holes (holes)
            (loop :while (and holes (= (first holes) lower)) :do
                 (incf lower)
                 (pop holes))
            (loop :while (and holes (= (lastcar holes) upper)) :do
                 (decf upper)
                 (setf holes (butlast holes)))
            holes)))
    (map nil (lambda+ ((direction pivot polarity))
               (ecase direction
                 (< (if polarity
                        (minf upper (1- pivot))
                        (maxf lower     pivot)))
                 (= (if polarity
                        (progn
                          (minf upper pivot)
                          (maxf lower pivot))
                        (when (not (member pivot holes))
                          (setf holes (merge 'list (list pivot) holes '<)))))
                 (> (if polarity
                        (maxf lower (1+ pivot))
                        (minf upper     pivot)))))
         tests)
    (when holes
      (setf holes (apply-holes (filter-holes holes))))
    (when (<= lower upper)
      (values lower upper holes))))

(defun possible-pivots (candidates)
  (loop :with result = '()
     :for (low . high) :in candidates
     :unless (eq low '*) :do
       (when (> low 0)
         (pushnew (1- low) result))
       (pushnew low      result)
       (pushnew (1+ low) result)
     :unless (eq high '*) :do
       (when (> high 0)
         (pushnew (1- high) result))
       (pushnew high      result)
       (pushnew (1+ high) result)
     :finally (return result)))

(defmethod map-possible-tests ((interface      interval-dispatch)
                               (function       function)
                               (previous-tests list)
                               (candidates     list))
  (map-product
   (lambda (direction pivot)
     (let ((test         (list direction pivot t))
           (test/negated (list direction pivot nil)))
       (when (and ; (or (not (eq direction '=)) (length= 1 candidates))
              (combined-test-bounds (list* test         previous-tests))
              (combined-test-bounds (list* test/negated previous-tests)))
         (funcall function test test/negated))))
   '(< = >) (possible-pivots candidates)))

(defmethod tests-info ((interface interval-dispatch)
                       (tests     list))
  (list (multiple-value-list (combined-test-bounds tests))))

(defmethod classify-candidate ((interface  interval-dispatch)
                               (tests      list)
                               (candidate  cons))
  (assert (not (eq (car candidate) '*))) ; TODO remove later
  (let+ (((low . high) candidate)
         (high (if (eq high '*) most-positive-fixnum high))
         ((&values lower upper forbidden)
          (combined-test-bounds tests))
         ((&flet point-forbidden? (point)
            (member point forbidden)))
         ((&flet range-forbidden? (low high)
            (loop :for point :from low :to high
               :always (point-forbidden? point)))))
    (cond
      ((<= low lower upper high)
       t)
      ((or (< high lower) (< upper low)
           (range-forbidden? low high))
       nil)
      (t
       :undetermined))))

(defmethod emit-test ((interface interval-dispatch)
                      (test      cons))
  (let+ (((&accessors-r/o number-var) interface)
         ((direction pivot polarity) test)
         (base-form `(,direction ,number-var ,pivot)))
    (if polarity
        base-form
        `(not ,base-form))))
