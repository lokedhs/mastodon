(in-package :mastodon-gui)

(defclass user-info-view (clim:view)
  ())

(defclass displayed-user ()
  ((user  :initarg :user
          :reader displayed-user/user)
   (image :initform nil
          :accessor displayed-user/image)))

(defun find-avatar (user min-width)
  "Return the smallest avatar for USER that is equal or larger than MIN-WIDTH"
  (loop
    with found = nil
    with found-width = nil
    for avatar in (status-net:author/avatar user)
    for width = (status-net:avatar/width avatar)
    when (or (not found)
             (and (>= width min-width)
                  (< width found-width))
             (and (< width min-width)
                  (> width found-width)))
      do (progn
           (setq found avatar)
           (setq found-width width))
    finally (return (status-net:avatar/url found))))

(defun extract-mastodon-account-info (user)
  (list (mastodon:account/display-name user)
        (mastodon:account/url user)
        (mastodon:account/note user)
        (mastodon:account/avatar user)))

(defun extract-status-net-account-info (user)
  (list (status-net:author/name user)
        (status-net:author/uri user)
        (status-net:author/summary user)
        (find-avatar user 128)))

(defun display-user-info (frame stream)
  (alexandria:when-let ((displayed-user (mastodon-frame/displayed-user frame)))
    (let ((user (displayed-user/user displayed-user)))
      (destructuring-bind (display-name url note avatar)
          (etypecase user
            (mastodon:account (extract-mastodon-account-info user))
            (status-net:author (extract-status-net-account-info user)))
        (when avatar
          (find-image-from-url (mastodon-frame/image-cache frame)
                               avatar
                               (lambda (entry immediate-p)
                                 (declare (ignore immediate-p))
                                 (setf (displayed-user/image displayed-user) (image-cache-entry/pixmap entry)))))
        (alexandria:when-let ((image (displayed-user/image displayed-user)))
          (clim:draw-pattern* stream image 0 0)
          (format stream "~%"))
        (clim:with-text-size (stream 20)
          (format stream "~a" display-name))
        (when note
          (format stream "~%~%")
          (present-html-string note stream))
        (format stream "~%~%")
        (present-to-stream (make-instance 'text-link-string :content url :href url) stream)))))

(defclass activity-list-view (clim:view)
  ())

(defun display-activity-list (frame stream)
  (let ((messages (mastodon-frame/messages frame)))
    (loop
      for msg in messages
      for first = t then nil
      unless first
        do (format stream "~%")
      do (present-to-stream msg stream))))

(clim:define-application-frame mastodon-frame ()
  ((credentials    :initarg :credentials
                   :accessor mastodon-frame/credentials)
   (displayed-user :initform nil
                   :accessor mastodon-frame/displayed-user)
   (messages       :type list
                   :initform nil
                   :accessor mastodon-frame/messages)
   (image-cache    :type image-cache
                   :reader mastodon-frame/image-cache))
  (:panes (activity-list :application
                         :default-view (make-instance 'activity-list-view)
                         :display-function 'display-activity-list)
          (user-info :application
                     :default-view (make-instance 'user-info-view)
                     :display-function 'display-user-info)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default (clim:horizontally ()
                       (3/10 user-info)
                       (7/10 activity-list))
                     bottom-adjuster
                     interaction-pane)))

(defmethod initialize-instance :after ((obj mastodon-frame) &key)
  (setf (slot-value obj 'image-cache) (make-instance 'image-cache)))

(defun current-cred (&optional (frame clim:*application-frame*))
  (mastodon-frame/credentials frame))

(defun load-timeline (category local-p)
  (let ((timeline (mastodon:timeline category :cred (current-cred) :local local-p)))
    (setf (mastodon-frame/messages clim:*application-frame*) timeline)))

(defun load-user-from-url (url cred)
  (cond ((mastodon:user-local-p url :cred cred)
         (mastodon:account-from-url url))
        (t
         (car (status-net:load-feed url)))))

(define-mastodon-frame-command (home-timeline :name "Home Timeline")
    ()
  (load-timeline "home" nil))

(define-mastodon-frame-command (public-timeline :name "Public Timeline")
    ()
  (load-timeline "public" nil))

(define-mastodon-frame-command (home-local :name "Home Local")
    ()
  (load-timeline "home" t))

(define-mastodon-frame-command (public-local :name "Public Local")
    ()
  (load-timeline "public" t))

(define-mastodon-frame-command (load-user :name "Show User")
    ((url 'string))
  (let ((user (load-user-from-url url (current-cred))))
    (setf (mastodon-frame/displayed-user clim:*application-frame*) (make-instance 'displayed-user :user user))))

(define-mastodon-frame-command (open-url :name "Open URL")
    ((url 'string))
  (bordeaux-threads:make-thread (lambda ()
                                  (uiop/run-program:run-program (list "xdg-open" url)))))

(define-mastodon-frame-command (post-new :name "Post New")
    ((text 'string))
  (mastodon:post text))

(clim:define-presentation-to-command-translator select-url
    (text-link open-url mastodon-frame)
    (obj)
  (list (text-link/href obj)))

(clim:define-presentation-to-command-translator select-user
    (user-ref load-user mastodon-frame)
    (obj)
  (list (user-ref/url obj)))

(clim:define-presentation-to-command-translator select-mention-link
    (mention-link load-user mastodon-frame)
    (obj)
  (list (user-ref/url obj)))

(defun mastodon-gui ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 10)))
  (let ((frame (clim:make-application-frame 'mastodon-frame
                                            :credentials mastodon:*credentials*
                                            :width 700 :height 500
                                            :left 10 :top 10)))
    (clim:run-frame-top-level frame)))
