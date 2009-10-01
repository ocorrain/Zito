(in-package "ZITO")

(defgeneric make-size-widget (render-type label slot-value stream))

(define-model image (medium)
    ((sizes :render-hint #'make-size-widget
	    :handler #'handle-sizes
	    :initform *default-sizes*
	    :attribute t)
     (thumbnails-last-created)))

(define-model captioned-image (image)
  ((title :index t :attribute t)
   (caption :render-hint 'textarea :attribute t :index t)))

(defun strconcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun get-size-prefix (symbol)
  (strconcat "size%" (string-downcase (symbol-name symbol))))

(defmethod make-size-widget ((render-type (eql 'html)) label slot-value stream)
  (with-html-output (s stream)
    (dolist (size slot-value)
      (destructuring-bind (size-type x y) size
	(htm (:fieldset
	      (:legend (str (string-capitalize (symbol-name size-type))))
	      (let* ((size-prefix (get-size-prefix size-type))
		     (xlabel (strconcat size-prefix "-x"))
		     (ylabel (strconcat size-prefix "-y")))
		(htm ((:label :for xlabel)
		      (fmt "X"))
		     (:input :type "text" :size "3" :name xlabel :id xlabel :value x)
		     ((:label :for ylabel)
		      (fmt "Y"))
		     (:input :type "text" :size "3" :name ylabel :id ylabel :value y)))))))))

(defvar *default-sizes* '((full-size 640 480)
			  (half-size 320 240)
			  (thumbnail 200 100)))



(defmethod handle-medium :after ((object image) slot-name parameters)
  (when (> (get-last-upload object)
	   (get-thumbnails-last-created object))
    (make-thumbnails object)))

(defun make-date-widget ())