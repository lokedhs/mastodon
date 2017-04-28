(asdf:defsystem #:mastodon
  :description "Common Lisp mastodon client"
  :license "Apache"
  :serial t
  :depends-on (:drakma
               :cxml
               :xpath
               :mcclim
               :closure-html
               :closer-mop
               :string-case
               :bordeaux-threads
               :yason
               :cl-ppcre
               :uiop
               :lparallel
               :local-time
               :status-net)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "json")
                             (:file "mastodon")
                             (:file "clim-misc")
                             (:file "format")
                             (:file "gui-misc")
                             (:file "image-cache")
                             (:file "gui")))))

