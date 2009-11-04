(in-package "ZITO")

(defun get-url (object)
  (format nil "/node/~A/~A"
	  (string-downcase (symbol-name (type-of object)))
	  (get-model-id object)))

(defun get-new-url (object)
  (format nil "/new/~A" (string-downcase (symbol-name (type-of object)))))

(defun render-form (instance stream &optional new)
  (with-html-output (s stream)
    ((:form :action (if new (get-new-url instance) (get-url instance))
	    :method :post :enctype "multipart/form-data")
     (:input :type "hidden" :name "id" :value (get-model-id instance))
     (:fieldset (:legend (fmt "Editing ~A"
			      (string-downcase (symbol-name (type-of instance)))))
		(dolist (attr (get-attributes (type-of instance)))
       (render-slot 'html attr (slot-value instance attr) stream)))
     (:fieldset
      ((:label :for "published")
       (str "Publicly viewable?")
       (if (get-published instance)
	   (htm (:input :type "checkbox" :name "published" :id "published" :checked "yes"))
	   (htm (:input :type "checkbox" :name "published" :id "published")))
       (:input :type "submit" :value "Save")
       (:input :type "reset" :value "Reset"))))))


(defun handle-post (instance parameters)
  (dolist (attr (get-attributes (symbol-name (type-of instance))))
    (handle-slot instance attr parameters)))

(defmethod render-with-hint ((render-type (eql 'html)) hint label slot-name slot-value stream)
  (typecase hint
      (function (funcall hint render-type label slot-value stream))
      (t (with-html-output (s stream)
	   (htm ((:label :for slot-name) (str label))
		(case hint
		  (text (htm (:input :type "text" :name slot-name :id slot-name :value slot-value)))
		  (textarea (htm ((:textarea :id slot-name :name slot-name) (str slot-value))))
		  (file (htm (:input :type "file" :name slot-name :id slot-name)))
		  (t (error "Unknown hint spec: ~A" hint))))))))

(define-condition instance-not-found (error)
  ((id :initarg :id :reader id)
   (instance-class :initarg :class :reader instance-class))
  (:report (lambda (condition stream)
	     (format stream "Instance with ID ~A and class ~A not found"
		     (id condition) (instance-class condition)))))

(define-condition bad-request (error)
  ((request-key :initarg :request-key :reader request-key)
   (request-value :initarg :request-value :reader request-value))
  (:report (lambda (condition stream)
	     (format stream "Bad request.  Key ~S. Value ~S"
		     (request-key condition) (request-value condition)))))

(defun get-object-from-url (url)
  (let ((sequence (split-sequence:split-sequence #\/ url :remove-empty-subseqs t)))
    (if (= (length sequence) 3)
	(destructuring-bind (node class-name instance-number)
	    sequence
	  (if (equal node "node")
	      (let* ((class-symbol (intern (string-upcase class-name)))
		     (class (ignore-errors (find-class class-symbol))))
		(if class
		    (let ((id (read-from-string instance-number)))
		      (if (numberp id)
			  (let ((instance (get-instance-by-value class 'model-id id)))
			    (if instance
				instance
				(error 'instance-not-found :id id :class class-symbol)))
			  (error 'bad-request :request-key "model-id" :request-value id)))
		    (error 'bad-request :request-key "class-name" :request-value class-name)))
	      (error 'bad-request :request-key "node-identifier" :request-value node)))
	(error 'bad-request :request-key "url" :request-value url))))

(defun get-new-object-type-from-url (url)
  (let ((sequence (split-sequence:split-sequence #\/ url :remove-empty-subseqs t)))
    (if (= (length sequence) 2)
	(destructuring-bind (new class-name) sequence
	  (if (equal new "new")
	      (let* ((class-symbol (intern (string-upcase class-name)))
		     (class (ignore-errors (find-class class-symbol))))
		(if (and class (member class-symbol *models*))
		    class-symbol
		    (error 'bad-request :request-key "class-name" :request-value class-name)))
	      (error 'bad-request :request-key "new-identifier" :request-value new)))
	(error 'bad-request :request-key "url" :request-value url))))





