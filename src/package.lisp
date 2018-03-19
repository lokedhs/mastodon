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
   #:post
   #:login
   #:request-new-application-id
   #:account/note
   #:account/avatar
   #:status/created-at
   #:account/acct
   #:status/url
   #:status/id
   #:search-from-site
   #:credentials
   #:credentials/url
   #:credentials/token
   #:favourite
   #:unfavourite
   #:reblog
   #:unreblog
   #:status/favourited
   #:status/reblogged
   #:stream-public
   #:stream-user
   #:stream-hashtag
   #:load-notifications
   #:notification
   #:notification/id
   #:notification/type
   #:notification/created-at
   #:notification/account
   #:notification/status
   #:load-status))

(defpackage :mastodon-gui
  (:use :cl)
  (:documentation "CLIM interface for mastodon")
  (:export #:mastodon-gui))
