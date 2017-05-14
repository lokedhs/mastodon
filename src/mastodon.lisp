(in-package :mastodon)

(defmacro assert-format (regexp value)
  (let ((value-sym (gensym)))
    `(let ((,value-sym ,value))
       (unless (cl-ppcre:scan ,regexp ,value-sym)
         (error "Illegal format for ~s" ,value-sym)))))

(defun json-request (url &key (method :get) additional-headers parameters)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request url
                           :method method
                           :additional-headers additional-headers
                           :parameters parameters
                           :want-stream t
                           :force-binary t)
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error "HTTP error when sending request: ~a. Reason: ~a" code reason-string))
           (let ((result (yason:parse (flexi-streams:make-flexi-stream stream :external-format :utf-8))))
             result))
      (when need-close
        (close stream)))))

(defun ensure-url (url)
  (if (eql (aref url (1- (length url))) #\/)
      url
      (concatenate 'string url "/")))

(defun request-new-application-id (url)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request (format nil "~aapi/v1/apps" (ensure-url url))
                           :method :post
                           :parameters '(("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                         ("client_name" . "status-net-lisp")
                                         ("scopes" . "read write follow")))
    (declare (ignore return-headers url-reply stream need-close))
    (unless (= code 200)
      (error "HTTP error when requesting application id: ~a. Reason: ~a" code reason-string))
    (let ((result (yason:parse (babel:octets-to-string content :encoding :utf-8))))
      (list (gethash "client_id" result)
            (gethash "client_secret" result)))))

(defclass credentials ()
  ((url    :type string
           :initarg :url
           :reader credentials/url)
   (token  :type string
           :initarg :token
           :reader credentials/token)))

(defvar *credentials* nil)

(defun login (url client-id client-secret username password)
  (unless (and (stringp url)
               (plusp (length url)))
    (error "Incorrect url: ~s" url))
  (let* ((prefix (ensure-url url))
         (result (json-request (format nil "~aoauth/token" prefix)
                               :method :post
                               :parameters `(("client_id" . ,client-id)
                                             ("client_secret" . ,client-secret)
                                             ("grant_type" . "password")
                                             ("username" . ,username)
                                             ("password" . ,password)
                                             ("scope" . "read write follow")))))
    (make-instance 'credentials
                   :url prefix
                   :token (gethash "access_token" result))))

(defun make-mastodon-url (url cred)
  (format nil "~a~a" (credentials/url cred) url))

(defun authenticated-http-request (url cred &key (method :get) additional-headers parameters)
  (json-request (make-mastodon-url url cred)
                :method method
                :additional-headers (cons (cons :authorization (format nil "Bearer ~a" (credentials/token cred)))
                                          additional-headers)
                :parameters parameters))

(defclass account ()
  ((acct            :json-field "acct"
                    :reader account/acct)
   (avatar          :json-field "avatar"
                    :initform nil
                    :reader account/avatar)
   (avatar-static   :json-field "avatar_static")
   (created-at      :json-field "created_at")
   (display-name    :json-field "display_name"
                    :reader account/display-name)
   (followers-count :json-field "followers_count")
   (following-count :json-field "following_count")
   (header          :json-field "header")
   (header-static   :json-field "header_static")
   (id              :json-field "id"
                    :reader account/id)
   (locked          :json-field "locked")
   (note            :json-field "note"
                    :reader account/note)
   (statuses-count  :json-field "statuses_count")
   (url             :json-field "url"
                    :reader account/url)
   (username        :json-field "username"
                    :reader account/username))
  (:metaclass json-entity-class))

(defmethod print-object ((obj account) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID ~s USERNAME ~s" (account/id obj) (account/username obj))))

(defun ensure-user-id (user)
  (etypecase user
    (account (account/id user))
    (string user)))

(defclass status ()
  ((id                     :json-field "id"
                           :reader status/id)
   (uri                    :json-field "uri")
   (url                    :json-field "url"
                           :reader status/url)
   (account                :json-field "account"
                           :json-type (:map account)
                           :reader status/account)
   (in-reply-to-id         :json-field "in_reply_to_id")
   (in-reply-to-account-id :json-field "in_reply_to_account_id")
   (reblog                 :json-field "reblog")
   (content                :json-field "content"
                           :reader status/content)
   (created-at             :json-field "created_at"
                           :reader status/created-at)
   (reblogs-count          :json-field "reblogs_count")
   (favourites-count       :json-field "favourites_count")
   (reblogged              :json-field "reblogged"
                           :reader status/reblogged)
   (favourited             :json-field "favourited"
                           :reader status/favourited)
   (sensistive             :json-field "sensistive"
                           :json-default-value nil)
   (spoiler-text           :json-field "spoiler_text")
   (visibility             :json-field "visibility")
   (media-attachments      :json-field "media_attachments")
   (mentions               :json-field "mentions")
   (tags                   :json-field "tags")
   (application            :json-field "application"))
  (:metaclass json-entity-class))

(defmethod print-object ((obj status) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ID ~s" (status/id obj))))

(defun load-account (user-id &key (cred *credentials*))
  (assert-format "^[0-9]+$" user-id)
  (parse-json-object 'account (authenticated-http-request (format nil "api/v1/accounts/~a" user-id) cred)))

(defun load-current-user (&key (cred *credentials*))
  (parse-json-object 'account (authenticated-http-request "api/v1/accounts/verify_credentials" cred)))

(defun load-following (user &key (cred *credentials*))
  (let ((user-id (ensure-user-id user)))
    (mapcar (lambda (v)
              (parse-json-object 'account v))
            (authenticated-http-request (format nil "api/v1/accounts/~a/following" user-id) cred))))

(defun follow-from-url (url &key (cred *credentials*))
  (authenticated-http-request "api/v1/follows"
                              cred
                              :method :post
                              :parameters `(("uri" . ,(status-net:load-webfinger url)))))

(defun update-follow (user follow-p &key (cred *credentials*))
  (authenticated-http-request (format nil "api/v1/accounts/~a/~:[unfollow~;follow~]"
                                      (ensure-user-id user) follow-p)
                              cred
                              :method :post))

(defun timeline (category &key local (cred *credentials*))
  (unless (member category '("home" "public") :test #'equal)
    (error "Invalid timeline category"))
  (let ((result (authenticated-http-request (format nil "api/v1/timelines/~a" category) cred
                                            :parameters (if local '(("local" . "true"))))))
    (mapcar (lambda (v)
              (parse-json-object 'status v))
            result)))

(defun user-local-p (url &key (cred *credentials*))
  "Given a user URL, return true if the user is local to this instance."
  (equal (puri:uri-host (puri:uri (credentials/url cred)))
         (puri:uri-host (puri:uri url))))

(defun account-from-url (url &key (cred *credentials*))
  (let* ((json (authenticated-http-request "api/v1/search" cred
                                          :parameters `(("q" . ,url))))
         (accounts (gethash "accounts" json)))
    (cond ((null accounts)
           nil)
          ((null (cdr accounts))
           (parse-json-object 'account (car accounts)))
          (t
           (error "More than one account found for url: ~s" url)))))

(defun post (text &key (cred *credentials*) reply-id sensitive sensitive-text (visibility :public))
  (check-type text string)
  (check-type reply-id (or null integer))
  (check-type sensitive-text (or null string))
  (check-type visibility (member :direct :private :unlisted :public))
  (let ((result (authenticated-http-request "api/v1/statuses" cred
                                            :method :post
                                            :parameters `(("status" . ,text)
                                                          ("visibility" . ,(ecase visibility
                                                                             (:direct "direct")
                                                                             (:private "private")
                                                                             (:unlisted "unlisted")
                                                                             (:public "public")))
                                                          ,@(if reply-id `(("in_reply_to_id" . ,(princ-to-string reply-id))))
                                                          ,@(if sensitive `(("sensitive" . "true")))
                                                          ,@(if sensitive-text `(("spolier_text" . ,sensitive-text)))))))
    (parse-json-object 'status result)))

(defun favourite (message-id &key (cred *credentials*))
  (check-type message-id integer)
  (authenticated-http-request (format nil "api/v1/statuses/~a/favourite" message-id)
                              cred
                              :method :post))

(defun unfavourite (message-id &key (cred *credentials*))
  (check-type message-id integer)
  (authenticated-http-request (format nil "api/v1/statuses/~a/unfavourite" message-id)
                              cred
                              :method :post))

(defun reblog (message-id &key (cred *credentials*))
  (check-type message-id integer)
  (authenticated-http-request (format nil "api/v1/statuses/~a/reblog" message-id)
                              cred
                              :method :post))

(defun unreblog (message-id &key (cred *credentials*))
  (check-type message-id integer)
  (authenticated-http-request (format nil "api/v1/statuses/~a/unreblog" message-id)
                              cred
                              :method :post))

(defun public-stream (callback-fn &key (cred *credentials*))
  (multiple-value-bind (content code return-headers url-reply stream-ret need-close reason-string)
      (drakma:http-request (make-mastodon-url "api/v1/streaming/user" cred)
                           :want-stream t
                           :force-binary t
                           :additional-headers `((:authorization . ,(format nil "Bearer ~a" (credentials/token cred)))))
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error "HTTP error when connecting. id: ~a. Reason: ~a" code reason-string))
           (let ((stream (flexi-streams:make-flexi-stream stream-ret :external-format :utf-8)))
             (loop
               with event-type = nil
               for line = (read-line stream)
               do (let ((pos (position #\: line)))
                    (when (and pos (> pos 0))
                      (let ((tag (subseq line 0 pos))
                            (content (string-trim '(#\Space) (subseq line (1+ pos)))))
                        (cond ((and (null event-type)
                                    (equal tag "event"))
                               (setq event-type content))
                              ((and (not (null event-type))
                                    (equal tag "data"))
                               (funcall callback-fn event-type (yason:parse content))
                               (setq event-type nil))
                              (t
                               (warn "Unexpected event. tag=~s, content=~s, event-type=~s" tag content event-type)))))))))
      (when need-close
        (close stream-ret)))))

(defun search-from-site (url query &key resolve)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request (concatenate 'string (ensure-url url) "api/v1/search")
                           :want-stream t
                           :parameters `(("q" . ,query)
                                         ("resolve" . ,(if resolve "false" "true"))))
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error "HTTP error when connecting. id: ~a. Reason: ~a" code reason-string))
           (let ((json (yason:parse (flexi-streams:make-flexi-stream stream :external-format :utf-8))))
             `((:accounts . ,(mapcar (lambda (v) (parse-json-object 'account v)) (gethash "accounts" json)))
               (:hashtags . ,(gethash "hashtags" json))
               (:statuses . ,(mapcar (lambda (v) (parse-json-object 'status v)) (gethash "statuses" json))))))
      (when need-close
        (close stream)))))
