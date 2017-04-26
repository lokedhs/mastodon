(in-package :mastodon-gui)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun call-in-event-handler (frame fn)
  (clim:execute-frame-command frame `(funcall ,(lambda () (funcall fn))))
  nil)

(defmacro with-call-in-event-handler (frame &body body)
  `(call-in-event-handler ,frame (lambda () ,@body)))

(defun present-text-with-wordwrap (stream text)
  #+sbcl
  (let ((words #+sbcl(sb-unicode:words text)))
    (loop
      with pane-width = (clim:bounding-rectangle-width stream)
      for word in words
      do (let ((x (clim:cursor-position (clim:stream-text-cursor stream))))
           (when (> (+ x (clim:stream-string-width stream word)) pane-width)
             (format stream "~%"))
           (format stream word))))

  ;; No word wrap on non-sbcl platforms
  #-sbcl
  (format stream "~a" text))

(defun display-debug-wordwrap (frame stream)
  (declare (ignore frame))
  (let ((text (format nil "~{~r~^ ~}" (loop for i from 1 to 100 collect i))))
    (present-text-with-wordwrap stream text)))

(clim:define-application-frame debug-wordwrap ()
  ()
  (:panes (content :application
                   :default-view nil
                   :display-function 'display-debug-wordwrap))
  (:layouts (default content)))

(defun run-debug-wordwrap ()
  (clim:run-frame-top-level (clim:make-application-frame 'mastodon-gui::debug-wordwrap :width 300 :height 400)))
