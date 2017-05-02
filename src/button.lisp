(in-package :mastodon-gui)

(defparameter *button-border* (clim:make-rgb-color 0.7 0.7 0.7))
(defparameter *button-background* (clim:make-rgb-color 0.95 0.95 0.95))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass button ()
  ())

(defgeneric button/text (button)
  (:documentation "The button label"))

(defclass plain-button (button)
  ((text :initarg :text)))

(defmethod button/text ((button plain-button))
  (slot-value button 'text))

(clim:define-presentation-method clim:present (obj (type button) stream (view t) &key)
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (clim:surrounding-output-with-border (stream :ink *button-border*
                                               :background *button-background*
                                               :move-cursor nil
                                               :padding 2)
    (format stream "~a" (button/text obj)))
  (clim:stream-increment-cursor-position stream 8 0))
