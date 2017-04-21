(in-package :mastodon)

(defclass json-entity-class (standard-class)
  ())

(defmethod closer-mop:validate-superclass ((class json-entity-class) (superclass standard-object))
  t)

(defclass json-entity-class-slot-definition-mixin ()
  ((json-field         :initarg :json-field
                       :accessor json-entity-class/json-field)
   (json-type          :initarg :json-type
                       :accessor json-entity-class/json-type)
   (json-default-value :initarg :json-default-value
                       :accessor json-entity-class/json-default-value)))

(defclass json-entity-class-direct-slot-definition (json-entity-class-slot-definition-mixin
                                                    closer-mop:standard-direct-slot-definition)
  ())

(defclass json-entity-class-effective-slot-definition (json-entity-class-slot-definition-mixin
                                                       closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class json-entity-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-entity-class-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class json-entity-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-entity-class-effective-slot-definition))

(defun ensure-slot-value (instance field-name &optional default-value)
  "Returns the value of slot FIELD-NAME in INSTANCE. If the slot is unbound, return DEFAULT-VALUE."
  (if (and (slot-exists-p instance field-name)
           (slot-boundp instance field-name))
      (slot-value instance field-name)
      default-value))

(defmethod closer-mop:compute-effective-slot-definition ((class json-entity-class) slot-name direct-slots)
  (let ((result (call-next-method)))
    (setf (json-entity-class/json-field result) (ensure-slot-value (car direct-slots) 'json-field nil))
    (setf (json-entity-class/json-type result) (ensure-slot-value (car direct-slots) 'json-type nil))
    (setf (json-entity-class/json-default-value result) (ensure-slot-value (car direct-slots) 'json-default-value :error))
    result))

(defun remove-keyword-from-list (arg-list keyword)
  (when arg-list
    (nconc (unless (eq (car arg-list) keyword)
             (list (car arg-list) (cadr arg-list)))
           (remove-keyword-from-list (cddr arg-list) keyword))))

(macrolet ((init-reinit (name)
             `(defmethod ,name :around ((class json-entity-class)
                                        &rest args
                                        &key direct-superclasses)
                #+nil(let ((root-class (find-class 'json-entity)))
                  (cond ((or (equal class root-class)
                             (member root-class direct-superclasses))
                         (call-next-method))
                        (t
                         (apply #'call-next-method class
                                :direct-superclasses (append (list root-class) direct-superclasses)
                                (remove-keyword-from-list args :direct-superclasses)))))
                ;; Class-level initialisations
                (declare (ignore args direct-superclasses))
                (call-next-method))))
  (init-reinit initialize-instance)
  (init-reinit reinitialize-instance))

(defun parse-json-value (json type)
  (if (null type)
      json
      (error "Currently unable to parse complex types")))

(defun parse-json-object (class json)
  (let* ((class (if (symbolp class)
                    (find-class class)
                    class))
         (obj (make-instance class)))
    (unless (typep class 'json-entity-class)
      (error "Class must be a json entity class"))
    (loop
      for slot in (closer-mop:class-slots class)
      for field = (json-entity-class/json-field slot)
      when field
        do (let ((value (gethash field json :not-defined)))
             (setf (closer-mop:slot-value-using-class class obj slot)
                   (if (eq value :not-defined)
                       (json-entity-class/json-default-value slot)
                       (parse-json-value value (json-entity-class/json-type slot))))))
    obj))

(defclass foo ()
  ((foo :json-field "foo")
   (bar :json-field "bar"))
  (:metaclass json-entity-class))

(defun test-foo ()
  (let* ((j (yason:parse "{\"foo\":\"some text\",\"bar\":10}")))
    (parse-json-object 'foo j)))
