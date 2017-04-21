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
               :cl-ppcre)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "json")
                             (:file "mastodon")))))
