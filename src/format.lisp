(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *link-colour* (clim:make-rgb-color 0.0 0.0 1.0))

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

(defmethod user-ref/url ((obj user-link))
  (mastodon:account/url (user-link/account obj)))

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
  (format stream "~a" (mastodon:account/display-name (user-link/account obj))))

(defun resolve-class-from-style (style)
  (let ((styles (split-sequence:split-sequence #\Space style)))
    (if (and (member "mention" styles :test #'equal)
             (member "h-card" styles :test #'equal))
        'mention-link
        'text-link-html)))

(defun parse-link (node)
  (let ((href (dom:get-attribute node "href")))
    (make-instance (resolve-class-from-style (dom:get-attribute node "class"))
                   :content (dom:child-nodes node)
                   :href href
                   :title (nil-if-empty (dom:get-attribute node "title")))))

(defun present-element (node stream)
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
        (t
         (present-node-list (dom:child-nodes node) stream))))))

(defun present-node (node stream)
  (cond ((dom:text-node-p node)
         (princ (dom:node-value node) stream))
        ((dom:element-p node)
         (present-element node stream))))

(defun present-node-list (nodes stream)
  (loop
    for node across nodes
    do (present-node node stream)))

(defun present-html-string (s stream)
  (let ((doc (closure-html:parse s (cxml-dom:make-dom-builder))))
    (status-net:with-html-namespaces
      (let ((body (xpath:first-node (xpath:evaluate "/h:html/h:body" doc))))
        (present-node-list (dom:child-nodes body) stream)))))

(clim:define-presentation-method clim:present (obj (type mastodon:status) stream (view t) &key)
  (let ((account (mastodon:status/account obj)))
    (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
      (present-to-stream (make-instance 'user-link
                                        :account account)
                         stream))
    (format stream "~%")
    (present-html-string (mastodon:status/content obj) stream)
    (format stream "~%")))
