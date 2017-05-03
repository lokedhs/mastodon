(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *link-colour* (clim:make-rgb-color 0.0 0.0 1.0))
(defparameter *status-heading-colour* (clim:make-rgb-color 0.2 0.4 0.2))

(defclass generic-status ()
  ())

(defgeneric generic-status-cache-value (msg)
  (:documentation "Returns the cache value for a given message"))

(defclass displayed-status (generic-status)
  ((status :initarg :status
           :reader displayed-status/status)))

(defmethod generic-status-cache-value ((obj displayed-status))
  (mastodon:status/url (displayed-status/status obj)))

(defclass message-actions-mixin ()
  ((msg :initarg :msg)))

(defclass reply-button (button message-actions-mixin)
  ())

(defmethod button/text ((button reply-button))
  "Reply")

(defclass boost-button (button message-actions-mixin)
  ())

(defmethod button/text ((button boost-button))
  "Boost")

(defclass remote-status (generic-status)
  ((user :initarg :user
         :reader remote-status/user)
   (post :initarg :post
         :reader remote-status/post)))

(defmethod generic-status-cache-value ((obj remote-status))
  (status-net:post/id (remote-status/post obj)))

(defclass user-ref ()
  ())

(defgeneric user-ref/url (user-ref)
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

(defclass mention-link (text-link user-ref)
  ((content :initarg :content
            :reader mention-link/content)))

(defclass user-link (user-ref)
  ((account :initarg :account
            :reader user-link/account)))

(defclass user-link-remote (user-ref)
  ((author :initarg :author
           :reader user-link-remote/author)))

(defmethod user-ref/url ((obj user-link))
  (mastodon:account/url (user-link/account obj)))

(defmethod user-ref/url ((obj mention-link))
  (text-link/href obj))

(defmethod user-ref/url ((obj user-link-remote))
  (status-net:author/uri obj))

(defun render-link (stream content-callback)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (clim:with-drawing-options (stream :ink *link-colour*)
    (funcall content-callback)))

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
         (when (and hcard mention)
           (return 'mention-link)))
    finally (return 'text-link-html)))

(defun parse-link (node)
  (let ((href (dom:get-attribute node "href")))
    #+nil(format *debug-io* "href=~s, class=~s, type=~s~%" href (dom:get-attribute node "class") (resolve-class-from-node node))
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

(defun present-status (stream user-ptr user-id timestamp content message-id)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (present-to-stream user-ptr stream))
  (clim:with-drawing-options (stream :ink *status-heading-colour*)
    (format stream " - ~a - ~a~%" user-id (format-readable-date timestamp)))
  (present-html-string content stream)
  (format stream "~%~%  ")
  (present-to-stream (make-instance 'reply-button :msg message-id) stream)
  (present-to-stream (make-instance 'boost-button :msg message-id) stream)
  (format stream "~%"))

(clim:define-presentation-method clim:present (obj (type displayed-status) stream (view t) &key)
  (let* ((status (displayed-status/status obj))
         (account (mastodon:status/account status)))
    (present-status stream
                    (make-instance 'user-link :account account)
                    (mastodon:account/acct (mastodon:status/account status))
                    (mastodon:status/created-at status)
                    (mastodon:status/content status)
                    (mastodon:status/url status))))

(clim:define-presentation-method clim:present (obj (type displayed-status) stream (view clim:textual-view) &key)
  (let ((status (displayed-status/status obj)))
    (format stream "Post by ~a: ~a"
            (mastodon:account/acct (mastodon:status/account status))
            (mastodon:status/url status))))

(clim:define-presentation-method clim:present (obj (type remote-status) stream (view t) &key)
  (let ((user (remote-status/user obj))
        (msg (remote-status/post obj)))
    (present-status stream
                    (make-instance 'user-link-remote :author user)
                    (status-net:author/name user)
                    (status-net:post/published msg)
                    (status-net:post/content-html msg)
                    (status-net:post/id msg))))

(clim:define-presentation-method clim:present (obj (type remote-status) stream (view clim:textual-view) &key)
  (let ((user (remote-status/user obj))
        (msg (remote-status/post obj)))
    (format stream "Post by ~a: ~a" (status-net:author/name user) (status-net:post/id msg))))
