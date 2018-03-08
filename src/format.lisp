(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *link-colour* (clim:make-rgb-color 0.0 0.0 1.0))
(defparameter *status-heading-colour* (clim:make-rgb-color 0.2 0.4 0.2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  generic-status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass generic-activity ()
  ())

(defgeneric generic-activity/message-id (status))
(defgeneric generic-activity/url (status))

(defgeneric generic-activity-cache-value (msg)
  (:documentation "Returns the cache value for a given message"))

(defgeneric generic-activity-cache-id (msg)
  (:documentation "Returns the cache id for a given message"))

(defclass generic-status (generic-activity)
  ((image           :initarg :image
                    :initform nil
                    :accessor generic-status/image)
   (include-image-p :initarg :include-image-p
                    :initform t
                    :reader generic-status/include-image-p)))

(defgeneric generic-status/user-ptr (status))
(defgeneric generic-status/user-id (status))
(defgeneric generic-status/timestamp (status))
(defgeneric generic-status/content (status))
(defgeneric generic-status/image-url (status))
(defgeneric generic-status/reblogged-p (status))
(defgeneric generic-status/favourited-p (status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  displayed-status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass displayed-status (generic-status)
  ((status :initarg :status
           :reader displayed-status/status)))

(defmethod print-object ((obj displayed-status) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (let ((displayed-status (displayed-status/status obj)))
      (format stream "FROM ~s ID ~s"
              (mastodon:account/acct (mastodon:status/account displayed-status))
              (mastodon:status/url displayed-status)))))

(defmethod generic-status/user-ptr ((obj displayed-status))
  (make-instance 'user-link :account (mastodon:status/account (displayed-status/status obj))))

(defmethod generic-status/user-id ((obj displayed-status))
  (mastodon:account/acct (mastodon:status/account (displayed-status/status obj))))

(defmethod generic-status/timestamp ((obj displayed-status))
  (mastodon:status/created-at (displayed-status/status obj)))

(defmethod generic-status/content ((obj displayed-status))
  (mastodon:status/content (displayed-status/status obj)))

(defmethod generic-activity/message-id ((obj displayed-status))
  (mastodon:status/url (displayed-status/status obj)))

(defmethod generic-status/image-url ((obj displayed-status))
  (mastodon:account/avatar (mastodon:status/account (displayed-status/status obj))))

(defmethod generic-status/reblogged-p ((obj displayed-status))
  (mastodon:status/reblogged (displayed-status/status obj)))

(defmethod generic-status/favourited-p ((obj displayed-status))
  (mastodon:status/favourited (displayed-status/status obj)))

(defmethod generic-activity/url ((obj displayed-status))
  (mastodon:status/url (displayed-status/status obj)))

(defmethod generic-activity-cache-id ((obj displayed-status))
  (mastodon:status/url (displayed-status/status obj)))

(defmethod generic-activity-cache-value ((obj displayed-status))
  (list (mastodon:status/url (displayed-status/status obj))
        (if (generic-status/image obj) :image-loaded :no-image)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  remote-status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass remote-status (generic-status)
  ((user :initarg :user
         :reader remote-status/user)
   (post :initarg :post
         :reader remote-status/post)))

(defmethod print-object ((obj remote-status) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "FROM ~s ID ~s"
            (status-net:author/name (remote-status/user obj))
            (status-net:post/id (remote-status/post obj)))))

(defmethod generic-status/user-ptr ((obj remote-status))
  (make-instance 'user-link-remote :author (remote-status/user obj)))

(defmethod generic-status/user-id ((obj remote-status))
  (status-net:author/name (remote-status/user obj)))

(defmethod generic-status/timestamp ((obj remote-status))
  (status-net:post/published (remote-status/post obj)))

(defmethod generic-status/content ((obj remote-status))
  (status-net:post/content-html (remote-status/post obj)))

(defmethod generic-activity/message-id ((obj remote-status))
  (status-net:post/id (remote-status/post obj)))

(defmethod generic-status/image-url ((obj remote-status))
  (find-avatar (remote-status/user obj) 32))

(defmethod generic-status/reblogged-p ((obj remote-status))
  ;; In order to correctly read the value we'd need to force a
  ;; replication of the message first. This is probably a bit
  ;; overkill, so we'll just return false here.
  nil)

(defmethod generic-activity/url ((obj remote-status))
  (status-net:post/alternate-url (remote-status/post obj)))

(defmethod generic-status/favourited-p ((obj remote-status))
  nil)

(defmethod generic-activity-cache-id ((obj remote-status))
  (status-net:post/id (remote-status/post obj)))

(defmethod generic-activity-cache-value ((obj remote-status))
  (status-net:post/id (remote-status/post obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass message-actions-mixin ()
  ((msg :initarg :msg
        :reader button-message)))

(defclass reply-button (button message-actions-mixin)
  ())

(defmethod button/text ((button reply-button))
  "Reply")

(defclass reblog-button (button message-actions-mixin)
  ())

(defmethod button/text ((button reblog-button))
  "Boost")

(defclass unreblog-button (button message-actions-mixin)
  ())

(defmethod button/text ((button unreblog-button))
  "Remove Boost")

(defclass favourite-button (button message-actions-mixin)
  ())

(defmethod button/text ((button favourite-button))
  "Favourite")

(defclass unfavourite-button (button message-actions-mixin)
  ())

(defmethod button/text ((button unfavourite-button))
  "Remove Favourite")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wrapped-user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass wrapped-user ()
  ())

(defgeneric wrapped-user/url (wrapped-user)
  (:documentation "Returns the URL for the given user"))

(defclass text-link ()
  ((href    :initarg :href
            :reader text-link/href)
   (title   :initarg :title
            :initform nil
            :reader text-link/title)))

(defclass text-link-html (text-link)
  ((content :initarg :content
            :reader text-link-html/content)))

(defclass text-link-string (text-link)
  ((content :initarg :content
            :reader text-link-string/content)))

(defclass mention-link (text-link wrapped-user)
  ((content :initarg :content
            :reader mention-link/content)))

(defclass user-link (wrapped-user)
  ((account :initarg :account
            :reader user-link/account)))

(defclass user-link-remote (wrapped-user)
  ((author :initarg :author
           :reader user-link-remote/author)))

(defmethod wrapped-user/url ((obj user-link))
  (mastodon:account/url (user-link/account obj)))

(defmethod wrapped-user/url ((obj mention-link))
  (text-link/href obj))

(defmethod wrapped-user/url ((obj user-link-remote))
  (status-net:author/uri obj))

(defun render-link (stream content-callback)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (clim:with-drawing-options (stream :ink *link-colour*)
    (funcall content-callback)))

(clim:define-presentation-method clim:present (obj (type wrapped-user) stream view &key)
  (format stream "~a" (wrapped-user/url obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clim:define-presentation-method clim:present (obj (type text-link-html) stream (view t) &key)
  ;; Need this declaration here until this issue is fixed:
  ;; https://github.com/robert-strandh/McCLIM/issues/156
  (render-link stream (lambda () (present-node-list (text-link-html/content obj) stream))))

(clim:define-presentation-method clim:present (obj (type text-link-string) stream (view t) &key)
  ;; Need this declaration here until this issue is fixed:
  ;; https://github.com/robert-strandh/McCLIM/issues/156
  (render-link stream (lambda () (format stream "~a" (text-link-string/content obj)))))

(clim:define-presentation-method clim:present (obj (type mention-link) stream (view t) &key)
  (render-link stream (lambda () (present-node-list (mention-link/content obj) stream))))

(clim:define-presentation-method clim:present (obj (type user-link) stream (view t) &key)
  (let* ((acct (user-link/account obj))
         (display-name (mastodon:account/display-name acct)))
    (format stream "~a" (if (and display-name (plusp (length display-name)))
                            display-name
                            (mastodon:account/acct acct)))))

(clim:define-presentation-method clim:present (obj (type user-link-remote) stream (view t) &key)
  (render-link stream (lambda () (format stream "~a" (status-net:author/display-name (user-link-remote/author obj))))))

(defun resolve-class-from-node (node)
  (loop
    with hcard = nil
    with mention = nil
    for curr = node then (dom:parent-node curr)
    until (equal (dom:node-name curr) "html")
    do (let ((style (split-sequence:split-sequence #\Space (dom:get-attribute curr "class"))))
         (when (member "h-card" style :test #'equal)
           (setf hcard t))
         (when (member "mention" style :test #'equal)
           (setf mention t))
         (when hcard #+nil(and hcard mention)
           (return 'mention-link)))
    finally (return 'text-link-html)))

(defun parse-link (node)
  (let ((href (dom:get-attribute node "href")))
    (make-instance (resolve-class-from-node node)
                   :content (dom:child-nodes node)
                   :href href
                   :title (nil-if-empty (dom:get-attribute node "title")))))

(defun present-element (node stream first)
  (labels ((present-element-bold ()
             (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
               (present-node-list (dom:child-nodes node) stream)))
           (present-element-italics ()
             (clim:with-text-style (stream (clim:make-text-style nil :italic nil))
               (present-node-list (dom:child-nodes node) stream))))
    (let ((name (dom:node-name node)))
      (string-case:string-case (name)
        ("b" (present-element-bold))
        ("i" (present-element-italics))
        ("a" (present-to-stream (parse-link node) stream))
        ("p" (progn
               (unless first
                 (format stream "~%~%"))
               (present-node-list (dom:child-nodes node) stream)))
        (t
         (present-node-list (dom:child-nodes node) stream))))))

(defun present-node (node stream first)
  (cond ((dom:text-node-p node)
         (present-text-with-wordwrap stream (dom:node-value node)))
        ((dom:element-p node)
         (present-element node stream first))))

(defun present-node-list (nodes stream)
  (loop
    for node across nodes
    for first = t then nil
    do (present-node node stream first)))

(defun present-html-string (s stream)
  (let ((doc (closure-html:parse s (cxml-dom:make-dom-builder))))
    (status-net:with-html-namespaces
      (let ((body (xpath:first-node (xpath:evaluate "/h:html/h:body" doc))))
        (present-node-list (dom:child-nodes body) stream)))))

(defun present-status (stream status)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((user-ptr (generic-status/user-ptr status))
        (user-id (generic-status/user-id status))
        (timestamp (generic-status/timestamp status))
        (content (generic-status/content status)))
    ;;
    (when (generic-status/include-image-p status)
      ;; Check if the image needs to be loaded
      (unless (generic-status/image status)
        (let ((image-url (generic-status/image-url status)))
          (when image-url
            (let ((frame clim:*application-frame*))
              (find-image-from-url (mastodon-frame/image-cache frame)
                                   image-url
                                   (lambda (entry immediate-p)
                                     (labels ((update-entry ()
                                                (setf (generic-status/image status) (image-cache-entry/pixmap entry))))
                                       (if immediate-p
                                           (update-entry)
                                           (with-call-in-event-handler frame
                                             (update-entry)
                                             (setf (clim:pane-needs-redisplay stream) t)
                                             (clim:redisplay-frame-pane (clim:pane-frame stream) stream))))))))))
      ;;
      (let ((image (generic-status/image status)))
        (when image
          (multiple-value-bind (x y)
              (clim:stream-cursor-position stream)
            (clim:draw-pattern* stream image x y)
            (clim:stream-increment-cursor-position stream 0 (+ (clim:pattern-height image) 10))))))
    ;;
    (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
      (present-to-stream user-ptr stream))
    (clim:with-drawing-options (stream :ink *status-heading-colour*)
      (format stream " - ~a - ~a~%" user-id (format-readable-date timestamp)))
    (present-html-string content stream)
    (format stream "~%~%  ")
    (present-to-stream (make-instance 'reply-button :msg status) stream)
    (present-to-stream (if (generic-status/reblogged-p status)
                           (make-instance 'unreblog-button :msg status)
                           (make-instance 'reblog-button :msg status))
                       stream)
    (present-to-stream (if (generic-status/favourited-p status)
                           (make-instance 'unfavourite-button :msg status)
                           (make-instance 'favourite-button :msg status))
                       stream)
    (format stream "~%")))

(clim:define-presentation-method clim:present (obj (type generic-status) stream (view t) &key)
  (present-status stream obj))

(clim:define-presentation-method clim:present (obj (type generic-status) stream (view clim:textual-view) &key)
  (format stream "Post: ~a" (generic-activity/message-id obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notification activities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass notification-activity (generic-activity)
  ((id :initarg :id
       :reader notification-activity/id)))

(defmethod generic-activity-cache-id ((obj notification-activity))
  (princ-to-string (notification-activity/id obj)))

(defmethod generic-activity-cache-value ((obj notification-activity))
  (princ-to-string (notification-activity/id obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; displayed-mention
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass displayed-mention (notification-activity)
  ((status :initarg :status
           :reader displayed-mention/status)))

(defmethod generic-activity/message-id ((obj displayed-mention))
  (mastodon:status/id (displayed-status/status (displayed-mention/status obj))))

(defmethod generic-activity/url ((obj displayed-mention))
  (mastodon:status/url (displayed-status/status (displayed-mention/status obj))))

(clim:define-presentation-method clim:present (obj (type displayed-mention) stream (view t) &key)
  (format stream "Mentioned~%~%")
  (present-status stream (displayed-mention/status obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; displayed-reblog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass displayed-reblog (notification-activity)
  ((user   :initarg :user
           :reader displayed-reblog/user)
   (status :initarg :status
           :reader displayed-reblog/status)))

(clim:define-presentation-method clim:present (obj (type displayed-reblog) stream (view t) &key)
  (present-to-stream (make-instance 'user-link :account (displayed-reblog/user obj)) stream)
  (format stream " boosted your status~%~%")
  (present-status stream (displayed-reblog/status obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; content parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun user-link->name (url)
  (puri:uri))

(defun find-mentions-in-content (text)
  (let ((doc (closure-html:parse s (cxml-dom:make-dom-builder))))
    (status-net:with-html-namespaces
      (let ((nodes (xpath:evaluate "//h:a/@href" doc))
            (refs (xpath:map-node-set->list (lambda (node)
                                              (user-link->name (dom:node-value node)))
                                            nodes)))
        (remove-duplicates (remove nil refs) :test #'equal)))))
