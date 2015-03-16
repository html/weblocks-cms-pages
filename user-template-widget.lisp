(in-package :weblocks-cms-pages)

(defwidget user-template-widget ()
           ((template :initarg :template :initform nil)
            (variables :initarg :variables :initform nil)))

(defmethod dom-classes ((widget user-template-widget))
  (format nil "~A user-template-~A" (call-next-method) (string-downcase (slot-value widget 'template))))

(defmethod print-object ((widget user-template-widget) stream)
  (if (and (slot-boundp widget 'template) 
           (slot-boundp widget 'variables))
    (with-slots (template variables) widget
      (print-unreadable-object (widget stream :type t :identity t)
        (format stream " \"~A\" template=~A ~A" 
                (dom-id widget)
                (string-downcase template)
                (if variables
                  (format nil "variables=~{~A~^,~}" (mapcar #'string-downcase (mapcar #'car (alexandria:plist-alist variables))))
                  "no variables"))))
    (call-next-method)))

(defun replace-variables (template variables)
  (let ((new-vars (append-default-variables template nil)))
    (append 
      (loop for (key . value) on variables by #'cddr 
            append (list key (or 
                               (getf new-vars key)
                               (getf variables key))))
      (loop for (key . value) on new-vars by #'cddr 
            append (unless (getf variables key)
                     (cons key value)))
      (list :tr (cons 
                  (lambda (str)
                    (weblocks::translate str))
                  "Функция для перевода, текст {{#tr}}Text{{/tr}} будет пропущен через переводчик")))))

(defmethod update-variables ((obj user-template-widget))
  (with-slots (template variables) obj 
    (setf variables (replace-variables template variables))))

(defmethod initialize-instance :after ((obj user-template-widget) &rest args)
  (update-variables obj))

(defmethod render-widget-body ((widget user-template-widget) &rest args)
  (with-slots (template variables) widget
    (let ((variables-with-widget-values 
            (loop for (key value) on variables by #'cddr 
                  append (list key (cons (cond 
                                           ((stringp (car value)) (car value))
                                           ((arrayp (car value)) (error "Don't use arrays ! Too much complexity"))
                                           ((subtypep (type-of (car value)) 'weblocks:widget)
                                            (weblocks::capture-weblocks-output (render-widget (car value))))
                                           (t (car value)))
                                         (cdr value))))))
      (write-string 
        (apply #'render-user-template (list* template variables-with-widget-values))
        *weblocks-output-stream*))))

(defmethod render-widget-children ((widget user-template-widget) &rest args)
  (declare (ignore widget args)))

(defmethod widget-children ((obj user-template-widget) &optional type)
  (with-slots (variables) obj
    (or 
      (slot-value obj 'weblocks::children)
      (setf 
        (slot-value obj 'weblocks::children)
        (loop for (key value) on variables by #'cddr 
              append (cond 
                       ((stringp (car value))
                        (list (make-widget (car value))))
                       ((subtypep (type-of (car value)) 'weblocks:widget) 
                        (list (car value)))))))))

(defun get-variables-descriptions (args)
  (yaclml:with-yaclml-output-to-string 
    (loop for (first second . third) in (alexandria:plist-alist args)
          do 
          (<:span 
            (<:format "{{~A}}" (string-downcase first))
            " - "
            (<:as-is third))
          (<:br))))

(defun render-user-template (name &rest args)
  "Render either {name}-{current-language} template ('test-tpl-en', 'test-tpl-ru') 
   or {name} template. If template does not exist, creates it and stores to database."
  (let ((wt-keyword (alexandria:make-keyword (string-upcase name)))
        (wt-symbol-name (intern (string-upcase name) *package*)))
    (weblocks-util:deftemplate wt-keyword wt-symbol-name)
    (weblocks-util::nested-html-part 
      (list :type :user-template :template-name name)
      (let ((template-obj (or 
                            ; First searching locale file with '-en' suffix or other depending on current-locale
                            (first-by-values 'weblocks-cms::template 
                                             :name (cons (string-downcase (format nil "~A-~A" name (weblocks::current-locale))) #'string=))
                            ; Then searching for file itself without siffixes
                            (first-by-values 'weblocks-cms::template 
                                             :name (cons (string-downcase name) #'string=))
                            ; Then creating if not found
                            (weblocks:persist-object weblocks-stores:*default-store* 
                                            (make-instance 'weblocks-cms::template :name (string-downcase name))))))
        (eval 
          `(defun ,wt-symbol-name (&rest args)
             (mustache:render* 
               ,(or (slot-value template-obj 'weblocks-cms::text) "")
               (loop for (first second) on args by #'cddr
                     collect (cons first second)))))
        (prog1 
          (eval 
            `(apply 
               #'weblocks-util:render-wt-to-string 
               (list* ,wt-keyword nil 
                      '( ,@(loop for (first second . third) 
                                 in (alexandria:plist-alist args)
                                 append (list first second))))))
          (setf (slot-value template-obj 'weblocks-cms::variables-descriptions)
                (get-variables-descriptions args))
          (setf (slot-value template-obj 'weblocks-cms::last-used-time) (get-universal-time)))))))

(defvar *template-widget-variables* nil)

(defun add-default-template-widget-variable (template-name variable-name variable-description &optional callback)
  (setf *template-widget-variables* 
        (remove-if 
          (lambda (item)
            (and (equal (first item) template-name)
                 (equal (second item) variable-name)))
          *template-widget-variables*))
  (push (list template-name variable-name variable-description callback) *template-widget-variables*))

(defun append-default-variables (template vars)
  (append 
    (loop for (template-name variable-name variable-description callback) in *template-widget-variables* 
          if (or 
               (equal template-name template)
               (progn 
                 (string=  (string-downcase (format nil "~A-~A" template-name (weblocks::current-locale)))
                           (string-downcase template))))
          append (list variable-name (cons (when callback (funcall callback)) variable-description)))
    vars))

(defun get-variable-description (template variable)
  "Returns description for given template and variable"
  (loop for (template-name variable-name variable-description callback) in *template-widget-variables* 
        if (and (equal template-name template)
                (equal variable variable-name))
        do (return-from get-variable-description variable-description)))

(defun make-template-widget (template &rest args)
  (make-instance 'user-template-widget 
                 :template template
                 :variables args))

(defun make-template-widget-from-model-object (template model-object &rest args)
  (apply #'make-template-widget (append (list template) args (object-values-and-descriptions model-object))))

(defun object-values-and-descriptions (object)
  "Returns plist suitable for passing as variables params to `make-template-widget`"
  (let ((description (weblocks-cms::get-model-description 
                       (alexandria:make-keyword (class-name (class-of object))))))

    (flet ((get-field-description (field)
             (loop for i in (getf description :fields) 
                   if (equal (getf i :name) field)
                   do (return-from get-field-description i))))

      (loop for i in (c2mop:class-slots (class-of object)) 
            append (let ((field-descr (get-field-description (alexandria:make-keyword (c2mop:slot-definition-name i)))))
                     (list
                       (alexandria:make-keyword (c2mop:slot-definition-name i))
                       (cons (slot-value object (c2mop:slot-definition-name i))
                             (or (getf field-descr :title) ""))))))))

(defun get-widget-by-template-name (template-name)
  (loop for i in (get-widgets-by-type 'user-template-widget) 
        do 
        (when (equal (slot-value i 'template) template-name)
          (return-from get-widget-by-template-name i))))
