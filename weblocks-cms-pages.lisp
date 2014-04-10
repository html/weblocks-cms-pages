;;;; weblocks-cms-pages.lisp

(in-package #:weblocks-cms-pages)

;;; "weblocks-cms-pages" goes here. Hacks and glory await!

(defvar *pages-ru-translation-table*
  '(("no name"  . "без имени")
    ("String"   . "Строка")
    ("Widget"   . "Виджет")
    ("empty"    . "пусто")
    ("Function" . "Функция")
    ("Template output" . "Вывод шаблона")
    ;
    ("Site Page" .  "Страница сайта")
    ("Page Content" .  "Содержимое")
    ("Site Path" .  "Путь на сайте")
    ("Parent Page" .  "Родительская страница")
    ("Page Title" .  "Название")
    ("Page Name" .  "Внутреннее название")))

(defmethod weblocks-cms:tree-item-title ((obj weblocks-cms::page))
  "Displaying page information string on site page gridedit"
  (format nil " ~A (~A)" 
          (if (weblocks-cms::page-name obj)
            (format nil "<b>~A</b>" (weblocks-cms::page-name obj))
            (format nil "<i>~A</i>" (translate "no name")))
          (with-slots (weblocks-cms::content) obj
            (cond 
              ((typep weblocks-cms::content 'weblocks-cms::template)
               (template-tree-path-pretty-print weblocks-cms::content))
              ((stringp weblocks-cms::content)
               (format nil "~A - <i>\"~A\"</i>" (translate "String") weblocks-cms::content))
              ((and 
                 (consp weblocks-cms::content)
                 (equal (car weblocks-cms::content)
                        :widget))
               (format nil "~A - <i>~A</i>" (translate "Widget") (get-widget-title-by-name (cdr weblocks-cms::content))))
              ((and 
                 (consp weblocks-cms::content)
                 (equal (car weblocks-cms::content)
                        :callback))
               (format nil "~A - <i>~A</i>" (translate "Function") (get-callback-title-by-name (cdr weblocks-cms::content))))
              (t "empty")))))

(defmethod template-tree-path-pretty-print ((obj weblocks-cms::template))
  "How template looks in content field of site page gridedit form"
  (format nil "~A - ~A" (translate "Template output") (weblocks-cms::template-name obj)))

(defmethod template-tree-path-pretty-print ((obj string))
  "How string looks in content field of site page gridedit form"
  obj)

(defmethod template-tree-path-pretty-print ((obj cons))
  "How widget looks in content field of site page gridedit form"
  (case (car obj)
    (:widget (format nil "~A - ~A" (translate "Widget") (get-widget-title-by-name (cdr obj))))
    (:callback (format nil "~A - ~A" (translate "Function") (get-callback-title-by-name (cdr obj))))
    (t (error "Not implemented work with cons ~A" obj))))

(defmethod weblocks-cms::get-view-fields-for-field-type-and-description :around (type description model-description-list)
  "Content field for site page gridedit form"
  (if (and 
        (equal (getf model-description-list :name) :page)
        (equal (getf description :name) :content))
    (progn 
      (setf (getf description :options) "template")
      (let ((relation-model-description-list (weblocks-cms::get-model-description-from-field-description-options description)))
        (list 
          (list 
            (weblocks-cms::keyword->symbol (getf description :name))
            :label (getf description :title)
            :present-as (list 
                          'bootstrap-typeahead 
                          :display-create-message nil
                          :choices 
                          (lambda (item)
                            (append 
                              (get-pages-widgets-titles)
                              (get-pages-callbacks-titles)
                              (loop for i in (all-of (weblocks-cms::keyword->symbol (getf relation-model-description-list :name)))
                                    collect (template-tree-path-pretty-print i)))))
            :reader (lambda (item)
                      (let ((item (slot-value item (weblocks-cms::keyword->symbol (getf description :name)))))
                        (and 
                          item
                          (template-tree-path-pretty-print item))))
            :writer (lambda (value item)
                      (let ((object (cond 
                                      ((ppcre:scan (translate "Template output") value)
                                       (ppcre:register-groups-bind 
                                         (name)
                                         ((format nil "~A~A" 
                                                  (translate "Template output")
                                                  "\\s+-\\s+(.*)$") value)
                                         (first-by-values 'weblocks-cms::template :name name)))
                                      ((ppcre:scan (translate "Widget") value)
                                       (ppcre:register-groups-bind 
                                         (title)
                                         ((format nil "~A~A"
                                                  (translate "Widget")
                                                  "\\s+-\\s+(.*)$") value)
                                         (cons :widget (get-widget-name-by-title title))))
                                      ((ppcre:scan (translate "Function") value)
                                       (ppcre:register-groups-bind 
                                         (title)
                                         ((format nil "~A~A" (translate "Function") "\\s+-\\s+(.*)$") value)
                                         (cons :callback (get-callback-name-by-title title))))
                                      (t value))))
                        (setf (slot-value item (weblocks-cms::keyword->symbol (getf description :name))) object)))))))
    (call-next-method)))

(defwidget pages-container (composite)
  ((selected :initform nil :accessor pages-container-selected))
  (:documentation "Widget that contains navigation stuff for pages functionality")
  (:default-initargs :uri-id :pages))

(defun get-page-content (page)
  (get-page-content-string 
    (weblocks-cms::page-content page)
    (weblocks-utils:find-by-values 'weblocks-cms::page :parent page)))

(defun make-routes-for-pages ()
  "Connects routes to pages-container widget"
  (clear-static-variables-routes-for-variable *routes-mapper* :pages.selected)
  (loop for i in (weblocks-utils:find-by-values 'weblocks-cms::page :parent nil) do 
        (connect *routes-mapper* 
                 (make-routes-with-static-value 
                   :pages.selected
                   `((,(weblocks-cms::page-path i) . ,(weblocks-cms::page-path i)))))))

(defmacro with-yaclml (&body body)
  "A wrapper around cl-yaclml with-yaclml-stream macro."
  `(yaclml:with-yaclml-stream *weblocks-output-stream*
     ,@body))

(defmethod render-widget ((obj pages-container) &rest args)
  (make-routes-for-pages)

  (with-yaclml 
    (<div :id (dom-id obj)
          (with-slots (selected) obj
            (when (and (not selected) 
                       (not (first-by-values 'weblocks-cms::page :path "/"))) 
              (return-from render-widget 
                           (mapcar #'render-widget (widget-children obj))))

            (let ((page (first-by-values 'weblocks-cms::page :path (or selected "/") :parent nil))
                  (page-content))
              (setf *current-page-title* (weblocks-cms::page-title page))
              (setf page-content (get-page-content page))
              (setf (slot-value obj 'weblocks::children) (list page-content))
              (render-widget page-content))
            (pushnew 
              (lambda ()
                (setf selected nil))
              (request-hook :request :post-render))))))

(defmethod widget-children ((obj pages-container) &optional type)
  (let ((ret (or (slot-value obj 'weblocks::children)
                 #+l(setf (slot-value obj 'weblocks::children) 
                          (list (get-page-content (first-by-values 'weblocks-cms::page :path (or (slot-value obj 'selected) "/") :parent nil)))))))
    (loop for i in ret 
          collect (if (listp i) 
                    (second i)
                    i))))

(defmethod get-page-content-string ((obj weblocks-cms::template) children)
  (let ((template (alexandria:make-keyword (string-upcase (weblocks-cms::template-name obj)))))
    (apply #'make-template-widget 
           (list* 
             template
             (loop for i in children 
                   append (let ((var (alexandria:make-keyword (string-upcase (weblocks-cms::page-name i)))))
                            (list 
                              var
                              (cons 
                                (get-page-content i)
                                (get-variable-description template var)))))))))

(defmethod get-page-content-string ((obj null) children)
  (format nil "~{~A~}"
          (loop for i in children 
                collect (get-page-content i))))

(defmethod get-page-content-string ((obj string) children)
  (format nil "~{~A~}" 
          (list* obj 
                 (loop for i in children 
                       collect (get-page-content i)))))

(defmethod get-page-content-string ((obj list) children)
  (if (equal (car obj) :widget)
    (make-page-widget-from-name (cdr obj) children)
    (call-next-method)))

(defvar *pages-widgets* nil 
  "Contains list of widget information. Each widget information piece contains widget name, widget title and widget callback")

(defun add-page-widget (name title callback)
  "Creates page widget,
   name is the internal name used as widget id,
   title is the title displayed in pages tree, 
   callback is the callback which should return widget"
  (push (list name title callback) *pages-widgets*)
  (setf *pages-widgets* (reverse (remove-duplicates (reverse *pages-widgets*) :key #'car))))

(defun get-pages-widgets-titles ()
  "Lists widget titles for displaying in dropdown list"
  (loop for (name title callback) in *pages-widgets* 
        collect (template-tree-path-pretty-print (cons :widget name))))

(defun get-widget-name-by-title (given-title)
  "Finds widget by title, returns its name"
  (loop for (name title callback) in *pages-widgets* 
        if (string= title given-title)
        do (return-from get-widget-name-by-title name)))

(defun get-widget-title-by-name (given-name)
  "Finds widget by name, returns its title"
  (loop for (name title callback) in *pages-widgets* 
        if (string= name given-name)
        do (return-from get-widget-title-by-name title)))

(defun make-page-widget-from-name (given-name children)
  "Finds widget by name, creates new instance of widget using children"
  (loop for (name title callback) in *pages-widgets* 
        if (equal name given-name)
        do (return-from make-page-widget-from-name (funcall callback given-name children))))

(defun render-page-by-name (name)
  "Finds page by name and renders it"
  (render-widget (get-page-content (first-by-values 'weblocks-cms::page :name (string-downcase name)))))

(defvar *pages-callbacks* nil 
  "Contains list of widget information. Each widget information piece contains widget name, widget title and widget callback")

(defun add-page-callback (name title callback)
  "Creates page callback,
   name is the internal name used as callback id,
   title is the title displayed in pages tree, 
   callback is the callback itself which should return something"
  (push (list name title callback) *pages-callbacks*)
  (setf *pages-callbacks* (reverse (remove-duplicates (reverse *pages-callbacks*) :key #'car))))

(defun get-pages-callbacks-titles ()
  "Lists widget titles for displaying in dropdown list"
  (loop for (name title callback) in *pages-callbacks* 
        collect (template-tree-path-pretty-print (cons :callback name))))

(defun get-callback-name-by-title (given-title)
  "Finds widget by title, returns its name"
  (loop for (name title callback) in *pages-callbacks* 
        if (string= title given-title)
        do (return-from get-callback-name-by-title name)))

(defun get-callback-title-by-name (given-name)
  "Finds widget by name, returns its title"
  (loop for (name title callback) in *pages-callbacks* 
        if (string= name given-name)
        do (return-from get-callback-title-by-name title)))

(defun funcall-page-callback (given-name children)
  "Finds widget by name, creates new instance of widget using children"
  (loop for (name title callback) in *pages-callbacks* 
        if (equal name given-name)
        do (return-from funcall-page-callback (funcall callback given-name children))))
