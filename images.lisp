(in-package "ZITO")

(defgeneric make-size-widget (render-type label slot-value stream))

(defvar *default-sizes* '((full-size 640 480)
			  (half-size 320 240)
			  (thumbnail 200 100)))

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


(defmethod handle-medium :after ((object image) slot-name parameters)
  (when (> (get-last-upload object)
	   (get-thumbnails-last-created object))
    (make-thumbnails object)))

(defun thumbnail-path (image size)
  (merge-pathnames
   (get-file image)
   (merge-pathnames
    (make-pathname :directory (list :relative (string-downcase (symbol-name size))))
    (merge-pathnames (get-media-path image)))))

(defun put-image-size (image size)
  (setf (get-sizes image)
	(remove-duplicates (cons size (get-sizes image)) 
			   :from-end t :key #'car)))

(defun get-resize-dimensions (image-x image-y box-x box-y)
  (if (and (< image-x box-y)
	   (< image-y box-y))
      (values image-x image-y)
      (apply #'values
	     (mapcar #'round
		     (if (> (/ image-x image-y) (/ box-x box-y))
			 (list box-x (* image-y (/ box-x image-x)))
			 (list (* image-x (/ box-y image-y)) box-y))))))

(defun resize-image-file (source-path dest-path box-x box-y)
  (cl-gd:with-image-from-file (img source-path)
    (multiple-value-bind (image-x image-y)
	(cl-gd:image-size img)
      (multiple-value-bind (new-x new-y)
	  (get-resize-dimensions image-x image-y box-x box-y)
	(cl-gd:with-image (new new-x new-y t)
	  (cl-gd:copy-image img new 0 0 0 0 image-x image-y
			    :resize t :resample t
			    :dest-width new-x :dest-height new-y)
	  (cl-gd:write-image-to-file dest-path
				     :if-exists :supersede
				     :image new))
	(values new-x new-y)))))

(defun make-thumbnails (image)
  (dolist (size (get-sizes image))
    (let ((thumbnail (thumbnail-path image (first size))))
      (ensure-directories-exist thumbnail)
      (multiple-value-bind (actual-x actual-y)
	  (resize-image-file (get-file image) thumbnail
			     (second size) (third size))
	(put-image-size image (list (first size) actual-x actual-y)))))
  (setf (get-thumbnails-last-created image) (get-universal-time)))
