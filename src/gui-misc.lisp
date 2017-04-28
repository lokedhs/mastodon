(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defun present-text-with-wordwrap (stream text)
  (let ((words (sb-unicode:words text)))
    (loop
      with pane-width = (- (clim:bounding-rectangle-width stream) 18) ;; add some extra space to accommodate the scrollbar
      for word in words
      do (let ((x (clim:cursor-position (clim:stream-text-cursor stream))))
           (when (> (+ x (clim:stream-string-width stream word)) pane-width)
             (format stream "~%"))
           (format stream "~a" word)))))

(defun present-multiline-with-wordwrap (text stream)
  (loop
    with start = nil
    for i from 0 below (length text)
    if (eql (aref text i) #\Newline)
      do (progn
           (when (and start (> i start))
             (present-text-with-wordwrap stream (subseq text start i))
             (setq start nil))
           (format stream "nl~%"))
    else
      do (progn
           (when (null start)
             (setq start i)))
    finally (when start
              (present-text-with-wordwrap stream (subseq text start)))))

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
