(in-package :weblocks-cms-pages)

(defun make-route-with-static-value (route spec value)
  (make-instance 
    'routes:route 
    :template (list 
                (make-instance 'routes::custom-variable-template 
                               :spec spec
                               :parse (lambda (url)
                                        (if (string= 
                                                (string-trim "/" url)
                                                (string-trim "/" route))
                                          value 
                                          ""
                                          ))))))

#+l(defun make-route-with-static-value (route varspecs)
  (routes:make-route 
    (apply #'concatenate 
           (list* 'string 
                  (string-right-trim "/" route)
                  "/"
                  (loop for (key value) on varspecs by #'cddr 
                        collect (format nil ":(~A)" (string-downcase key)))))
    (loop for (key value) on varspecs by #'cddr 
          append (list key 
                       (lambda (value-to-return &rest args)
                         (describe  value-to-return)
                         (when (zerop (length value-to-return))
                           value))))))

(let ((mapper (make-instance 'weblocks::priority-generation-mapper)))
  (connect mapper (make-route-with-static-value "/catalog" :main-selector.selected "test"))
  (multiple-value-bind (route params) (routes:match mapper "/catalog")
    (assert (not (null route)))
    (assert (equal params '((:main-selector.selected . "test"))))))

(let ((mapper (make-instance 'weblocks::priority-generation-mapper)))
  (connect mapper (make-route-with-static-value "/collections" :main-selector.selected "test"))
  (multiple-value-bind (route params) (routes:match mapper "/collections")
    (assert (not (null route)))
    (assert (equal params '((:main-selector.selected . "test"))))))

(defclass custom-static-variable-template (routes::custom-variable-template)
  ((variables-values :initform nil :initarg :values)))

(defclass route-with-static-variable (routes:route)
  ())

(defun clear-static-variables-routes-for-variable (mapper given-variable-name)
  (let (variable-name)
    (loop for (route . priority) in (slot-value mapper 'weblocks::routes) do 
          (when (typep route 'route-with-static-variable)
            (setf variable-name (routes:template-data (first (slot-value route 'routes::template))))
            (when (equal given-variable-name variable-name)
              (setf (slot-value (first (slot-value route 'routes::template)) 'variables-values) nil)
              (return-from clear-static-variables-routes-for-variable t))))))

(defmethod weblocks::connect ((mapper weblocks::priority-generation-mapper) (obj route-with-static-variable))
  "Connects static variable route with mapper, tries to find existing route with same variable name, if found changes it, if not connects route to mapper."
  (flet ((merge-route-variable-values (from-route to-route)
           (setf 
             (slot-value (first (slot-value to-route 'routes::template)) 'variables-values)
             (remove-duplicates 
               (append 
                 (slot-value (first (slot-value from-route 'routes::template)) 'variables-values)
                 (slot-value (first(slot-value to-route 'routes::template)) 'variables-values))
               :key #'car
               :test #'string=))))
    (let ((given-variable-name (routes:template-data (first (slot-value obj 'routes::template))))
          variable-name)
      (loop for (route . priority) in (slot-value mapper 'weblocks::routes) do 
        (when (typep route 'route-with-static-variable)
          (setf variable-name (routes:template-data (first (slot-value route 'routes::template))))
          (when (equal given-variable-name variable-name)
            (merge-route-variable-values obj route)
            (return-from weblocks::connect))))

      (call-next-method))))

(defun static-variable-template-parse-callback (template)
  (lambda (url)
    (loop for (key . val) in (slot-value template 'variables-values) 
          if (string= 
               (string-trim "/" url)
               (string-trim "/" key))
          return val)))

(defun make-routes-with-static-value (spec values)
  (make-instance 
    'route-with-static-variable 
    :template (list 
                (let ((template 
                        (make-instance 'custom-static-variable-template 
                                       :values values
                                       :spec spec)))
                  (setf (slot-value template 'routes::parse-fun)
                        (static-variable-template-parse-callback template))
                  template))))

(let ((mapper (make-instance 'routes:mapper)))

  (routes:connect mapper (make-routes-with-static-value 
                           :main-selector.selected
                           (list 
                             (cons "catalog" "catalog")
                             (cons "collections" "collections"))))

  (multiple-value-bind (route params) (routes:match mapper "/catalog")
    (assert (not (null route)))
    (assert (equal params '((:main-selector.selected . "catalog")))))

  (multiple-value-bind (route params) (routes:match mapper "/collections")
    (assert (not (null route)))
    (assert (equal params '((:main-selector.selected . "collections"))))))

