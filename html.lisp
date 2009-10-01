(in-package "ZITO")

(defun get-url (object)
  (format nil "/node/~A/~A"
	  (string-downcase (symbol-name (type-of object)))
	  (get-model-id object)))

(defun render-form (instance stream)
  (with-html-output (s stream)
    ((:form :action (get-url instance) :method :post :enctype "multipart/form-data")
     (:input :type "hidden" :name "id" :value (get-model-id instance))
     (:fieldset (:legend (fmt "Editing ~A"
			      (string-downcase (symbol-name (type-of instance)))))
		(dolist (attr (get-attributes (type-of instance)))
       (render-slot 'html attr (slot-value instance attr) stream))))))

(defun handle-post (instance parameters)
  (dolist (attr (get-attributes (symbol-name (type-of object))))
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

 