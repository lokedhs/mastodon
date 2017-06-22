(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defun nil-if-empty (s)
  (if (equal s "") nil s))

(defun parse-timestamp (s)
  (local-time:parse-timestring s))

(defun format-readable-date (date-string)
  (let* ((ts (parse-timestamp date-string))
         (now (local-time:now))
         (d (local-time:timestamp-difference now ts)))
    (cond ((< d 60)
           (let ((n (truncate d)))
             (format nil "~a second~@[s~] ago" n (/= n 1))))
          ((< d local-time:+seconds-per-hour+)
           (let ((n (round d local-time:+seconds-per-minute+)))
             (format nil "~a minute~@[s~] ago" n (/= n 1))))
          ((< d local-time:+seconds-per-day+)
           (let ((n (round d local-time:+seconds-per-hour+)))
             (format nil "~a hour~@[s~] ago" n (/= n 1))))
          (t
           (local-time:format-timestring nil ts :format '((:year 4) #\Space :short-month #\Space (:day 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating application pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass update-application-pane (clim:application-pane)
  ())

(defvar *in-redraw* nil)

(defmethod clim:note-sheet-region-changed :after ((sheet update-application-pane))
  (unless *in-redraw*
    (let ((*in-redraw* t))
      (clim:redisplay-frame-pane (clim:pane-frame sheet) sheet))))

(defun make-update-pane (name &rest args)
  (apply #'clim:make-clim-stream-pane :type 'update-application-pane
                                      :name name
                                      args))
