(cl:in-package #:dispatch-experiment)

;;; Protocol

(defgeneric map-possible-tests (interface function previous-tests candidates)
  (:documentation
   "Class FUNCTION with lambda-list

      (test negated-test)

    only tests that can in principle succeed"))

(defgeneric tests-info (interface tests)
  (:documentation
   "TODO"))

(defgeneric emit-test (interface test)
  (:documentation
   "Return code for performing TEST against VARIABLE."))

(defgeneric classify-candidate (interface tests candidate)
  (:documentation
   "TODO"))

;;; Default behavior

(defmethod map-possible-tests ((interface      t)
                               (function       t)
                               (previous-tests t)
                               (candidates     t))
  (let ((function (ensure-function function)))
    (map-possible-tests interface function previous-tests candidates)))

(defmethod tests-info ((interface t) (tests list))
  nil)

;;; Test selection

(defun partition-candidates (interface tests tests/first-negated candidates)
  (loop :for candidate :in candidates
     :for positive? = (classify-candidate interface tests candidate)
     :for negative? = (classify-candidate interface tests/first-negated candidate)

     :when (eq positive? t)
     :collect candidate :into definitely-positive
     :when (eq positive? :undetermined)
     :collect candidate :into possibly-positive

     :when (eq negative? t)
     :collect candidate :into definitely-negative
     :when (eq negative? :undetermined)
     :collect candidate :into possibly-negative

     :finally
       (assert (null (set-difference candidates (reduce #'union
                                                        (list definitely-positive possibly-positive
                                                              definitely-negative possibly-negative))))) ; TODO delete later
       (return (values definitely-positive possibly-positive
                       definitely-negative possibly-negative))))

(defun balance-and-reduction
    (test definitely-positive possibly-positive
     definitely-negative possibly-negative)
  (declare (ignore test))
  (+ (abs (- (+ (length definitely-positive) (length possibly-positive))
             (+ (length definitely-negative) (length possibly-negative))))
     (* 2 (length (intersection (append definitely-positive possibly-positive)
                                (append definitely-negative possibly-negative)
                                :test #'eq)))
     ;;
     (length possibly-positive) (length possibly-negative)
     ;;
     (if (and (emptyp possibly-positive) (emptyp definitely-negative) (emptyp possibly-negative)) 0 10000)
     ))

(defun choose-test (interface previous-tests candidates
                    &key
                      (score #'balance-and-reduction))
  (let ((score             (ensure-function score))
        (best-test         nil)
        (best-test/negated nil)
        (best-positive     nil)
        (best-negative     nil)
        (best-score        nil))
    (map-possible-tests
     interface
     (lambda (test test/negated)
       (let ((tests               (list* test         previous-tests))
             (tests/first-negated (list* test/negated previous-tests)))
         (let+ (((&values definitely-positive possibly-positive
                          definitely-negative possibly-negative)
                 (partition-candidates
                  interface tests tests/first-negated candidates))
                (score (funcall score test
                                definitely-positive possibly-positive
                                definitely-negative possibly-negative)))
           (when (or (not best-score) (< score best-score))
             (setf best-test         test
                   best-test/negated test/negated
                   best-positive     (append definitely-positive possibly-positive)
                   best-negative     (append definitely-negative possibly-negative)
                   best-score        score)))))
     previous-tests candidates)
    (values best-test best-test/negated
            best-positive best-negative
            best-score)))

;;; Decision tree

(defstruct (node (:copier nil))
  (interface nil :read-only t))

(defstruct (decision
             (:include node)
             (:constructor make-decision (interface test then else))
             (:copier nil))
  (test nil :read-only t)
  (then nil :read-only t)
  (else nil :read-only t))

(defstruct (leaf
             (:include node)
             (:constructor make-leaf (interface candidates &optional info))
             (:copier nil))
  (candidates nil :type list :read-only t)
  (info       nil            :read-only t))

(defun make-decision-tree (interface candidates
                           &key
                             (previous-tests '())
                             (score          #'balance-and-reduction)
                             (make-leaf      (lambda (interface tests candidates)
                                               (let ((info (tests-info interface tests)))
                                                 (make-leaf interface candidates info)))))
  (let+ ((make-leaf (ensure-function make-leaf))
         ((&flet make-leaf (tests candidates)
            (funcall make-leaf interface tests candidates)))
         ((&labels rec (tests candidates)
            ;; Stopping when there is only one candidate may leave out
            ;; final tests to decide whether the candidate actually
            ;; matches.
            (case (length candidates)
              (0
               (make-leaf tests candidates))
              (t
               (let+ (((&values test test/negated positive negative)
                       (choose-test interface tests candidates :score score)))
                 (cond
                   ((not test)
                    (make-leaf tests candidates))
                   ((equal positive negative)
                    (make-leaf tests candidates))
                   (t
                    (make-decision
                     interface
                     test
                     (rec (list* test         tests) positive)
                     (rec (list* test/negated tests) negative))))))))))
    (rec previous-tests candidates)))

(defun emit-decision-tree-code (tree cont)
  (let+ (((&labels rec (node)
            (etypecase node
              (decision
               (let+ (((&structure-r/o decision- interface test then else) node))
                 `(if ,(emit-test interface test)
                      ,(rec then)
                      ,(rec else))))
              (leaf
               (let+ (((&structure-r/o leaf- info candidates) node))
                 (funcall cont candidates info)))))))
    (rec tree)))

;;; Debugging

(defun print-decision-tree (tree &optional (stream *standard-output*))
  (utilities.print-tree:print-tree
   stream tree
   (utilities.print-tree:make-node-printer
    (lambda (stream depth node)
      (declare (ignore depth))
      (typecase node
        (decision (format stream "~A" (decision-test node)))
        (leaf     (format stream "{~{~A~^ ~}}" (leaf-candidates node)))))
    nil
    (lambda (node)
      (typecase node
        (decision (list (decision-then node) (decision-else node)))
        (leaf     '()))))))
