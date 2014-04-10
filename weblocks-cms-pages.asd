;;;; weblocks-cms-pages.asd

(asdf:defsystem #:weblocks-cms-pages
  :serial t
  :description "Static pages functionality for Weblocks CMS"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :depends-on (#:weblocks
               #:weblocks-cms 
               #:weblocks-utils 
               #:yaclml 
               #:weblocks-mustache-templates-editor)
  :components ((:file "package")
               (:file "make-route-with-static-value")
               (:file "user-template-widget")
               (:file "weblocks-cms-pages")))

