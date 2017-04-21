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
             (error "HTTP error when requesting application id: ~a. Reason: ~a" code reason-string))
           (let ((result (yason:parse (flexi-streams:make-flexi-stream stream :external-format :utf-8))))
             result))
      (when need-close
        (close stream)))))

(defun request-new-application-id ()
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request "https://mastodon.brussels/api/v1/apps"
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
  ((url   :type string
          :initarg :url
          :reader credentials/url)
   (token :type string
          :initarg :token
          :reader credentials/token)))

(defvar *credentials* nil)

(defun login (url client-id client-secret username password)
  (unless (and (stringp url)
               (plusp (length url)))
    (error "Incorrect url: ~s" url))
  (let* ((prefix (if (eql (aref url (1- (length url))) #\/)
                     url
                     (concatenate 'string url "/")))
         (result (json-request (format nil "~aoauth/token" prefix)
                               :method :post
                               :parameters `(("client_id" . ,client-id)
                                             ("client_secret" . ,client-secret)
                                             ("grant_type" . "password")
                                             ("username" . ,username)
                                             ("password" . ,password)))))
    (make-instance 'credentials
                   :url prefix
                   :token (gethash "access_token" result))))

(defun authenticated-http-request (url cred &key (method :get) additional-headers)
  (json-request (format nil "~a~a" (credentials/url cred) url)
                :method method
                :additional-headers (cons (cons :authorization (format nil "Bearer ~a" (credentials/token cred)))
                                          additional-headers)))

(defclass account ()
  ((acct            :json-field "acct")
   (avatar          :json-field "avatar")
   (avatar-static   :json-field "avatar_static")
   (created-at      :json-field "created_at")
   (display-name    :json-field "display_name")
   (followers-count :json-field "followers_count")
   (following-count :json-field "following_count")
   (header          :json-field "header")
   (header-static   :json-field "header_static")
   (id              :json-field "id")
   (locked          :json-field "locked")
   (note            :json-field "note")
   (statuses-count  :json-field "statuses_count")
   (url             :json-field "url")
   (username        :json-field "username"))
  (:metaclass json-entity-class))

(defun load-account (user-id &key (cred *credentials*))
  (assert-format "^[0-9]+$" user-id)
  (parse-json-object 'account (authenticated-http-request (format nil "api/v1/accounts/~a" user-id) cred)))
