(defpackage :mastodon
  (:use :cl)
  (:documentation "Mastodon client for Common Lisp")
  (:export
   #:timeline
   #:status
   #:status/account
   #:account
   #:account/display-name
   #:status/content
   #:*credentials*
   #:account-from-url
   #:user-local-p
   #:account/url
   #:load-account
   #:load-current-user
   #:load-following
   #:follow-from-url
   #:update-follow
   #:post))

(defpackage :mastodon-gui
  (:use :cl)
  (:documentation "CLIM interface for mastodon")
  (:export #:mastodon-gui))
