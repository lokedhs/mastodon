(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass user-info-view (clim:view)
  ())

(defclass displayed-user ()
  ((user  :initarg :user
          :reader displayed-user/user)
   (image :initform nil
          :accessor displayed-user/image)
   (timeline :initarg :timeline
             :initform nil
             :accessor displayed-user/timeline)))

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
        :html
        (mastodon:account/avatar user)))

(defun extract-status-net-account-info (user)
  (list (status-net:author/name user)
        (status-net:author/uri user)
        (status-net:author/summary user)
        (if (equal (status-net:author/summary-type user) "html") :html nil)
        (find-avatar user 128)))

(defun display-user-info (frame stream)
  (alexandria:when-let ((displayed-user (mastodon-frame/displayed-user frame)))
    (let ((user (displayed-user/user displayed-user)))
      (destructuring-bind (display-name url note note-type avatar)
          (etypecase user
            (mastodon:account (extract-mastodon-account-info user))
            (status-net:author (extract-status-net-account-info user)))
        (when avatar
          (find-image-from-url (mastodon-frame/image-cache frame)
                               avatar
                               (lambda (entry immediate-p)
                                 (labels ((update-user ()
                                            (setf (displayed-user/image displayed-user) (image-cache-entry/pixmap entry))))
                                   (if immediate-p
                                       (update-user)
                                       (with-call-in-event-handler frame
                                         (update-user)
                                         (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'user-info) :force-p t)))))))
        (alexandria:when-let ((image (displayed-user/image displayed-user)))
          (clim:draw-pattern* stream image 0 0)
          (clim:stream-increment-cursor-position stream 0 (+ (clim:pattern-height image) 10)))
        (clim:with-text-size (stream 20)
          (format stream "~a" display-name))
        (when note
          (format stream "~%~%")
          (if (eq note-type :html)
              (present-html-string note stream)
              (present-multiline-with-wordwrap note stream)))
        (format stream "~%~%")
        (present-to-stream (make-instance 'text-link-string :content url :href url) stream)
        ;;
        (alexandria:when-let ((timeline (displayed-user/timeline displayed-user)))
          (format stream "~%")
          (present-horizontal-separator stream)
          (present-activity-list timeline stream))
        (clim:scroll-extent stream 0 0)))))

(defclass activity-list-view (clim:view)
  ())

(defun present-horizontal-separator (stream)
  (let ((width (clim:bounding-rectangle-width stream)))
    (multiple-value-bind (x y)
        (clim:cursor-position (clim:stream-text-cursor stream))
      (declare (ignore x))
      (let ((new-y (+ y 10)))
        (clim:draw-line stream
                        (clim:make-point 20 new-y)
                        (clim:make-point (- width 20) new-y)))
      #+nil(setf (clim:cursor-position (clim:stream-text-cursor stream))
            (values x (+ 20 y)))
      (clim:stream-increment-cursor-position stream 0 20))))

(defun present-activity-list (messages stream)
  (loop
    for msg in messages
    for first = t then nil
    unless first
      do (present-horizontal-separator stream)
    do (clim:updating-output (stream :unique-id msg
                                     :id-test (lambda (a b)
                                                (and (eql (type-of a) (type-of b))
                                                     (equal (generic-status/message-id a) (generic-status/message-id b))))
                                     :cache-value (generic-status-cache-value msg)
                                     :cache-test (lambda (a b)
                                                   (equal a b)))
         (present-to-stream msg stream))))

(defun display-activity-list (frame stream)
  (present-activity-list (mastodon-frame/messages frame) stream)
  (clim:scroll-extent stream 0 0))

(defun display-post-message (frame stream)
  (declare (ignore frame))
  (format stream "Post message~%"))

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
                         :display-function 'display-activity-list
                         :display-time t)
          (user-info :application
                     :default-view (make-instance 'user-info-view)
                     :display-function 'display-user-info
                     :display-time t)
          (post-message-form :application
                             :display-function 'display-post-message)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default (clim:horizontally ()
                       (1/3 user-info)
                       (1/3 activity-list)
                       (1/3 post-message-form))
                     bottom-adjuster
                     interaction-pane)))

(defmethod initialize-instance :after ((obj mastodon-frame) &key)
  (setf (slot-value obj 'image-cache) (make-instance 'image-cache)))

(defun current-cred (&optional (frame clim:*application-frame*))
  (mastodon-frame/credentials frame))

(defun load-timeline (category local-p)
  (let ((frame clim:*application-frame*)
        (timeline (mastodon:timeline category :cred (current-cred) :local local-p)))
    (setf (mastodon-frame/messages frame)
          (mapcar (lambda (v)
                    (make-instance 'displayed-status :status v))
                  timeline))
    (let ((pane (clim:find-pane-named frame 'activity-list)))
      (setf (clim:pane-needs-redisplay pane) t))))

(defun replicate-remote-message (status cred)
  (check-type status remote-status)
  (let* ((result (mastodon:search-from-site (mastodon:credentials/url cred)
                                            (status-net:post/alternate-url (remote-status/post status))))
         (statuses (cdr (assoc :statuses result))))
    (unless (alexandria:sequence-of-length-p statuses 1)
      (error "Can't find post"))
    (first statuses)))

(defun reply-to-remote-post (orig text cred)
  (mastodon:post text :cred cred :reply-id (mastodon:status/id (replicate-remote-message orig cred))))

(define-mastodon-frame-command (home-timeline :name "Home")
    ()
  (load-timeline "home" nil))

(define-mastodon-frame-command (public-timeline :name "Public")
    ()
  (load-timeline "public" nil))

(define-mastodon-frame-command (public-local :name "Local")
    ()
  (load-timeline "public" t))

(define-mastodon-frame-command (load-user :name "Show User")
    ((url 'string))
  (let* ((frame clim:*application-frame*)
         (cred (mastodon-frame/credentials frame)))
    (setf (mastodon-frame/displayed-user frame)
          (cond ((mastodon:user-local-p url :cred cred)
                 (let ((user (mastodon:account-from-url url)))
                   (make-instance 'displayed-user :user user)))
                (t
                 (destructuring-bind (user feed)
                     (status-net:load-feed url)
                   (make-instance 'displayed-user :user user
                                                  :timeline (mapcar (lambda (msg)
                                                                      (make-instance 'remote-status
                                                                                     :user user
                                                                                     :post msg
                                                                                     :include-image-p nil))
                                                                    feed))))))
    (setf (clim:pane-needs-redisplay (clim:find-pane-named frame 'user-info)) t)))

(define-mastodon-frame-command (open-url :name "Open URL")
    ((url 'string))
  (bordeaux-threads:make-thread (lambda ()
                                  (uiop/run-program:run-program (list "xdg-open" url)))))

(define-mastodon-frame-command (post-new :name "Post")
    ((text 'string))
  (mastodon:post text))

(define-mastodon-frame-command (reply :name "Reply")
    ((in-reply-to 'generic-status)
     (text 'string))
  (etypecase in-reply-to
    (displayed-status (mastodon:post text
                                     :reply-id (mastodon:status/id (displayed-status/status in-reply-to))
                                     :cred (current-cred)))
    (remote-status (reply-to-remote-post in-reply-to text (current-cred)))))

(defun ensure-post-id (msg cred)
  (let ((status (etypecase msg
                  (displayed-status (displayed-status/status msg))
                  (remote-status (replicate-remote-message (remote-status/post msg) cred)))))
    (mastodon:status/id status)))

(define-mastodon-frame-command (reblog-post :name "Reblog")
    ((message 'generic-status))
  (let ((cred (current-cred)))
    (mastodon:reblog (ensure-post-id message cred) :cred cred)))

(define-mastodon-frame-command (unreblog-post :name "Unreblog")
    ((message 'generic-status))
  (let ((cred (current-cred)))
    (mastodon:unreblog (ensure-post-id message cred) :cred cred)))

(define-mastodon-frame-command (favourite-post :name "Favourite")
    ((message 'generic-status))
  (let ((cred (current-cred)))
    (mastodon:favourite (ensure-post-id message cred) :cred cred)))

(define-mastodon-frame-command (unfavourite-post :name "Unfavourite")
    ((message 'generic-status))
  (let ((cred (current-cred)))
    (mastodon:unfavourite (ensure-post-id message cred) :cred cred)))

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

(clim:define-presentation-to-command-translator select-boost
    (boost-button reblog-post mastodon-frame)
    (obj)
  (list (button-message obj)))

(clim:define-presentation-to-command-translator select-favourite
    (favourite-button favourite-post mastodon-frame)
    (obj)
  (list (button-message obj)))

(defvar *frame* nil)

(defun mastodon-gui ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 10)))
  (let ((frame (clim:make-application-frame 'mastodon-frame
                                            :credentials mastodon:*credentials*
                                            :width 1000 :height 700
                                            :left 10 :top 10)))
    (setq *frame* frame)
    (clim:run-frame-top-level frame)))
