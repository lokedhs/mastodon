(in-package :mastodon-gui)

(defclass user-info-view (clim:view)
  ())

(defun extract-mastodon-account-info (user)
  (list (mastodon:account/display-name user)
        (mastodon:account/url user)))

(defun extract-status-net-account-info (user)
  (list (status-net:author/name user)
        (status-net:author/uri user)))

(defun display-user-info (frame stream)
  (let ((user (mastodon-frame/displayed-user frame)))
    (when user
      (destructuring-bind (display-name url)
          (etypecase user
            (mastodon:account (extract-mastodon-account-info user))
            (status-net:author (extract-status-net-account-info user)))
        (clim:with-text-size (stream 20)
          (format stream "~a" display-name))
        (format stream "~%")
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
                   :accessor mastodon-frame/messages))
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

(defun current-cred (&optional (frame clim:*application-frame*))
  (mastodon-frame/credentials frame))

(defun load-timeline (category)
  (let ((timeline (mastodon:timeline category :cred (current-cred))))
    (setf (mastodon-frame/messages clim:*application-frame*) timeline)))

(defun load-user-from-url (url cred)
  (cond ((mastodon:user-local-p url :cred cred)
         (mastodon:account-from-url url))
        (t
         (car (status-net:load-feed url)))))

(define-mastodon-frame-command (home-timeline :name "Home Timeline")
    ()
  (load-timeline "home"))

(define-mastodon-frame-command (public-timeline :name "Public Timeline")
    ()
  (load-timeline "public"))

(define-mastodon-frame-command (load-user :name "Show User")
    ((url 'string))
  (let ((user (load-user-from-url url (current-cred))))
    (setf (mastodon-frame/displayed-user clim:*application-frame*) user)))

(clim:define-presentation-to-command-translator select-user
    (user-ref load-user mastodon-frame)
    (obj)
  (list (user-ref/url obj)))

(defun mastodon-gui ()
  (let ((frame (clim:make-application-frame 'mastodon-frame
                                            :credentials mastodon:*credentials*
                                            :width 700 :height 500
                                            :left 10 :top 10)))
    (clim:run-frame-top-level frame)))
