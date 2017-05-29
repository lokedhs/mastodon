(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass user-info-view (clim:view)
  ())

(defparameter *instance-list-filename* (merge-pathnames (user-homedir-pathname) #p"instances.txt"))
(defparameter *creds-filename* (merge-pathnames (user-homedir-pathname) #p"credentials.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generic-user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass generic-user ()
  ((image :initform nil
          :accessor generic-user/image)))

(defgeneric generic-user/display-name (user))
(defgeneric generic-user/url (user))
(defgeneric generic-user/note (user))
(defgeneric generic-user/avatar (user))
(defgeneric generic-user/timeline (user))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local mastodon users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass displayed-user (generic-user wrapped-user)
  ((user  :initarg :user
          :reader displayed-user/user)
   (timeline :initarg :timeline
             :initform nil
             :accessor displayed-user/timeline)))

(defmethod generic-user/display-name ((obj displayed-user))
  (mastodon:account/display-name (displayed-user/user obj)))

(defmethod generic-user/url ((obj displayed-user))
  (mastodon:account/url (displayed-user/user obj)))

(defmethod generic-user/note ((obj displayed-user))
  (list (mastodon:account/note (displayed-user/user obj)) :html))

(defmethod generic-user/avatar ((obj displayed-user))
  (mastodon:account/avatar (displayed-user/user obj)))

(defmethod wrapped-user/url ((obj displayed-user))
  (mastodon:account/url (displayed-user/user obj)))

(defmethod generic-user/timeline ((obj displayed-user))
  (displayed-user/timeline obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remote statusnet users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass remote-user (generic-user wrapped-user)
  ((user  :initarg :user
          :reader remote-user/user)
   (image :initform nil
          :accessor remote-user/image)
   (timeline :initarg :timeline
             :initform nil
             :accessor remote-user/timeline)))

(defmethod generic-user/display-name ((obj remote-user))
  (status-net:author/name (remote-user/user obj)))

(defmethod generic-user/url ((obj remote-user))
  (status-net:author/uri (remote-user/user obj)))

(defmethod generic-user/note ((obj remote-user))
  (let ((user (remote-user/user obj)))
    (list (status-net:author/summary user)
          (if (equal (status-net:author/summary-type user) "html") :html nil))))

(defmethod generic-user/avatar ((obj remote-user))
  (find-avatar (remote-user/user obj) 128))

(defmethod wrapped-user/url ((obj remote-user))
  (status-net:author/uri (remote-user/user obj)))

(defmethod generic-user/timeline ((obj remote-user))
  (remote-user/timeline obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass follow-user-button (button)
  ((user :initarg :user
         :reader follow-user-button/user)))

(defmethod button/text ((button follow-user-button))
  "Follow")

(defclass remove-cached-instance-button (button)
  ((instance :initarg :instance
             :reader remove-cached-instance-button/instance)))

(defmethod button/text ((button remove-cached-instance-button))
  "Remove")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stored instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stored-instance ()
  ((name   :initarg :name
           :reader stored-instance/name)
   (url    :initarg :url
           :reader stored-instance/url)
   (id     :initarg :id
           :reader stored-instance/id)
   (secret :initarg :secret
           :reader stored-instance/secret)))

(defun load-instances ()
  (let ((*read-eval* nil))
    (let ((in (open *instance-list-filename* :if-does-not-exist nil)))
      (when in
        (unwind-protect
             (let ((instances (read in)))
               (unless (listp instances)
                 (error "Unexpected format of instance list"))
               (mapcar (lambda (v)
                         (destructuring-bind (name url id secret)
                             v
                           (make-instance 'stored-instance :name name :url url :id id :secret secret)))
                       instances))
          (close in))))))

(defun save-instances (instances)
  (with-open-file (out *instance-list-filename* :direction :output :if-exists :supersede)
    (write (mapcar (lambda (v)
                     (list (stored-instance/name v)
                           (stored-instance/url v)
                           (stored-instance/id v)
                           (stored-instance/secret v)))
                   instances)
           :stream out)))

(clim:define-presentation-method clim:present (obj (type stored-instance) stream (view t) &key)
  (format stream "~a" (stored-instance/name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Saved credentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-creds ()
  (let ((*read-eval* nil))
    (let ((in (open *creds-filename* :if-does-not-exist nil)))
      (when in
        (unwind-protect
             (let ((content (read in)))
               (destructuring-bind (url token)
                   content
                 (make-instance 'mastodon:credentials :url url :token token)))
          (close in))))))

(defun save-creds (cred)
  (with-open-file (out *creds-filename* :direction :output :if-exists :supersede)
    (write (list (mastodon:credentials/url cred)
                 (mastodon:credentials/token cred))
           :stream out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun display-user-info (frame stream)
  (alexandria:when-let ((user (mastodon-frame/displayed-user frame)))
    (let ((display-name (generic-user/display-name user))
          (url (generic-user/url user))
          (avatar (generic-user/avatar user)))
      (when avatar
        (find-image-from-url (mastodon-frame/image-cache frame)
                             avatar
                             (lambda (entry immediate-p)
                               (labels ((update-user ()
                                          (setf (generic-user/image user) (image-cache-entry/pixmap entry))))
                                 (if immediate-p
                                     (update-user)
                                     (with-call-in-event-handler frame
                                       (update-user)
                                       (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'user-info) :force-p t)))))))
      (alexandria:when-let ((image (generic-user/image user)))
        (clim:draw-pattern* stream image 0 0)
        (clim:stream-increment-cursor-position stream 0 (+ (clim:pattern-height image) 10)))
      (clim:with-text-size (stream 20)
        (format stream "~a" display-name))
      (destructuring-bind (note note-type)
          (generic-user/note user)
        (when note
          (format stream "~%~%")
          (if (eq note-type :html)
              (present-html-string note stream)
              (present-multiline-with-wordwrap note stream))))
      (format stream "~%~%")
      (present-to-stream (make-instance 'text-link-string :content url :href url) stream)
      (format stream "~%~%")
      (present-to-stream (make-instance 'follow-user-button :user user) stream)
      ;;
      (alexandria:when-let ((timeline (generic-user/timeline user)))
        (format stream "~%")
        (present-horizontal-separator stream)
        (present-activity-list timeline stream))
      (clim:scroll-extent stream 0 0))))

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
                                                     (equal (generic-activity-cache-id a)
                                                            (generic-activity-cache-id b))))
                                     :cache-value (generic-activity-cache-value msg)
                                     :cache-test (lambda (a b)
                                                   (equal a b)))
         (present-to-stream msg stream))))

(defun display-activity-list (frame stream)
  (present-activity-list (mastodon-frame/messages frame) stream)
  (clim:scroll-extent stream 0 0))

(clim:define-application-frame mastodon-frame ()
  ((instances      :type list
                   :initform nil
                   :accessor mastodon-frame/instances)
   (credentials    :initarg :credentials
                   :accessor mastodon-frame/credentials)
   (displayed-user :initform nil
                   :accessor mastodon-frame/displayed-user)
   (messages       :type list
                   :initform nil
                   :accessor mastodon-frame/messages)
   (image-cache    :type image-cache
                   :reader mastodon-frame/image-cache))
  (:menu-bar mastodon-frame-command-table)
  (:panes (activity-list :application
                         :default-view (make-instance 'activity-list-view)
                         :display-function 'display-activity-list
                         :display-time t
                         :incremental-redisplay t)
          (user-info :application
                     :default-view (make-instance 'user-info-view)
                     :display-function 'display-user-info
                     :display-time t)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default (clim:horizontally ()
                       (1/2 user-info)
                       (1/2 activity-list))
                     bottom-adjuster
                     interaction-pane)))

(defmethod initialize-instance :after ((obj mastodon-frame) &key)
  (setf (slot-value obj 'image-cache) (make-instance 'image-cache))
  (let ((instances (load-instances)))
    (when instances
      (setf (mastodon-frame/instances obj) instances))))

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

(defun load-notifications-timeline ()
  (let ((frame clim:*application-frame*)
        (notifications (mastodon:load-notifications :cred (current-cred))))
    (setf (mastodon-frame/messages frame)
          (loop
            for msg in notifications
            for notification-id = (mastodon:notification/id msg)
            for user = (mastodon:notification/account msg)
            for created-at = (mastodon:notification/created-at msg)
            append (string-case:string-case ((mastodon:notification/type msg))
                     ("mention" (list (make-instance 'displayed-mention
                                                     :id notification-id
                                                     :status (make-instance 'displayed-status
                                                                            :status (mastodon:notification/status msg)))))
                     ("reblog" (list (make-instance 'displayed-reblog
                                                    :id notification-id
                                                    :user user
                                                    :status (make-instance 'displayed-status
                                                                            :status (mastodon:notification/status msg)))))
                     (t nil))))
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

(defun normalise-server-url (url)
  (when (zerop (length url))
    (error "Empty URL"))
  (unless (cl-ppcre:scan "^https?://" url)
    (setq url (format nil "https://~a" url)))
  (unless (alexandria:ends-with-subseq "/" url)
    (setq url (format nil "~a/" url)))
  url)

(clim:make-command-table 'mastodon-frame-command-table
                         :errorp nil
                         :menu '(("Home" :command home-timeline)
                                 ("Public" :command public-timeline)
                                 ("Local" :command public-local)
                                 ("Notifiations" :command notifications-timeline)))

(define-mastodon-frame-command (add-instance :name "Add Instance")
    ((url 'string))
  (let ((frame clim:*application-frame*)
        (normalised-url (normalise-server-url url)))
    (cond ((member normalised-url (mastodon-frame/instances frame) :key #'stored-instance/url :test #'equal)
           (format (clim:find-pane-named frame 'interaction-pane) "Instance already added~%"))
          (t
           (destructuring-bind (id secret)
               (mastodon:request-new-application-id normalised-url)
             (push (make-instance 'stored-instance :name normalised-url :url normalised-url :id id :secret secret) (mastodon-frame/instances frame))
             (save-instances (mastodon-frame/instances frame))
             (format (clim:find-pane-named frame 'interaction-pane) "Instance created~%"))))))

(define-mastodon-frame-command (list-instances :name "List Instances")
    ()
  (let* ((frame clim:*application-frame*)
         (stream (clim:find-pane-named frame 'interaction-pane)))
    (dolist (instance (mastodon-frame/instances frame))
      (present-to-stream instance stream)
      (format stream " ")
      (present-to-stream (make-instance 'remove-cached-instance-button :instance instance) stream)
      (format stream "~%"))))

(clim:define-presentation-to-command-translator remove-cached-instance
    (remove-cached-instance-button remove-instance mastodon-frame)
    (obj)
  (list (remove-cached-instance-button/instance obj)))

(define-mastodon-frame-command (remove-instance :name "Remove Instance")
    ((url 'stored-instance))
  (let ((frame clim:*application-frame*)
        (normalised-url (normalise-server-url url)))
    (cond ((not (member normalised-url (mastodon-frame/instances frame) :key #'car :test #'equal))
           (format (clim:find-pane-named frame 'interaction-pane) "Instance not found~%"))
          (t
           (setf (mastodon-frame/instances frame)
                 (remove normalised-url (mastodon-frame/instances frame) :key #'car :test #'equal))
           (save-instances (mastodon-frame/instances frame))
           (format (clim:find-pane-named frame 'interaction-pane) "Instance removed~%")))))

(define-mastodon-frame-command (notifications-timeline :name "Notifications")
    ()
  (load-notifications-timeline))

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
    ((user 'wrapped-user))
  (let* ((url (wrapped-user/url user))
         (frame clim:*application-frame*)
         (cred (mastodon-frame/credentials frame)))
    (setf (mastodon-frame/displayed-user frame)
          (cond ((mastodon:user-local-p url :cred cred)
                 (let ((user (mastodon:account-from-url url :cred cred)))
                   (make-instance 'displayed-user :user user)))
                (t
                 (destructuring-bind (user feed)
                     (status-net:load-feed url)
                   (make-instance 'remote-user :user user
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
  (let ((trimmed (string-trim '(#\Space #\Tab) text)))
    (if (zerop (length trimmed))
        (error "Empty message")
        (mastodon:post trimmed :cred (current-cred)))))

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

(define-mastodon-frame-command (follow-account :name "Follow")
    ((user 'wrapped-user))
  (let ((cred (current-cred)))
    (mastodon:follow-from-url (wrapped-user/url user) :cred cred)))

(define-mastodon-frame-command (authenticate :name "Authenticate")
    ((instance 'stored-instance)
     (user 'string)
     (password 'string))
  (let ((cred (mastodon:login (stored-instance/url instance) (stored-instance/id instance) (stored-instance/secret instance) user password)))
    (setf (mastodon-frame/credentials clim:*application-frame*) cred)
    (save-creds cred)))

(clim:define-presentation-to-command-translator select-url
    (text-link open-url mastodon-frame)
    (obj)
  (list (text-link/href obj)))

(clim:define-presentation-to-command-translator select-user
    (wrapped-user load-user mastodon-frame)
    (obj)
  (list obj))

(clim:define-presentation-to-command-translator select-mention-link
    (mention-link load-user mastodon-frame)
    (obj)
  (list obj))

(clim:define-presentation-to-command-translator select-reblog
    (reblog-button reblog-post mastodon-frame)
    (obj)
  (list (button-message obj)))

(clim:define-presentation-to-command-translator select-unreblog
    (unreblog-button unreblog-post mastodon-frame)
    (obj)
  (list (button-message obj)))

(clim:define-presentation-to-command-translator select-favourite
    (favourite-button favourite-post mastodon-frame)
    (obj)
  (list (button-message obj)))

(clim:define-presentation-to-command-translator select-unfavourite
    (unfavourite-button unfavourite-post mastodon-frame)
    (obj)
  (list (button-message obj)))

(clim:define-presentation-to-command-translator select-follow-user
    (follow-user-button follow-account mastodon-frame)
    (obj)
  (list (follow-user-button/user obj)))

(defvar *frame* nil)

(defun mastodon-gui ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 10)))
  (let ((frame (clim:make-application-frame 'mastodon-frame
                                            :credentials (or (load-creds) mastodon:*credentials*)
                                            :width 1000 :height 700
                                            :left 10 :top 10)))
    (setq *frame* frame)
    (clim:run-frame-top-level frame)))
