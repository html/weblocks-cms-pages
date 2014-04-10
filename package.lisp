;;;; package.lisp

(defpackage #:weblocks-cms-pages
  (:use #:cl)
  (:export #:pages-container 
           #:add-page-widget #:add-page-callback #:make-template-widget 
           #:get-variable-description #:add-default-template-widget-variable 
           #:render-page-by-name #:get-widget-by-template-name)
  (:import-from :weblocks 
                #:defwidget #:translate #:composite 
                #:*routes-mapper* #:connect #:render-widget 
                #:dom-id #:dom-classes #:render-widget-body 
                #:*weblocks-output-stream* #:request-hook #:render-widget-children 
                #:widget-children #:make-widget #:get-widgets-by-type)
  (:import-from :weblocks-utils #:first-by-values #:all-of))


(in-package :weblocks-cms-pages)

(weblocks-cms:def-additional-schema 
  :template
  `((:TITLE ,(translate "Site Page") :NAME :PAGE :FIELDS
     ((:TITLE ,(translate "Page Content") :NAME :CONTENT :TYPE :SINGLE-CHOICE :OPTIONS NIL)
      (:TITLE ,(translate "Site Path") :NAME :PATH :TYPE :STRING :OPTIONS NIL)
      (:TITLE ,(translate "Parent Page") :NAME :PARENT :TYPE :SINGLE-RELATION
       :OPTIONS NIL)
      (:TITLE ,(translate "Page Title") :NAME :TITLE :TYPE :STRING :OPTIONS NIL)
      (:TITLE ,(translate "Page Name") :NAME :NAME :TYPE :STRING :OPTIONS NIL)))))

