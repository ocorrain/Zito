(in-package "ZITO")

(defgeneric get-media-path (object &optional absolute)
  (:documentation "When ABSOLUTE is :WEB it returns a url-like path to
  the object's directory.  Otherwise when ABSOLUTE is T it returns the
  absolute path of the directory where a file object (and any
  ancillary file objects created in handling/transcoding etc) should
  be stored.  When ABSOLUTE is nil it returns the path of media files
  relative to the value stored under the key \"media-root\" in the
  active store controller."))

(defmethod get-media-path ((object medium) &optional absolute)
  (let ((relative-pathspec (list :relative
				 "media"
				 (string-downcase (symbol-name (type-of object)))
				 (format nil "~A" (get-model-id object)))))
    (let ((media-root (get-from-root "media-root")))
      (unless media-root (error "MEDIA-ROOT not set"))
      (let ((absolute-path (merge-pathnames
			    (make-pathname :directory relative-pathspec)
			    media-root)))
	(ensure-directories-exist absolute-path)
	(if absolute
	    (case absolute
	      (:web (merge-pathnames (make-pathname :directory relative-pathspec)
				     #p"/"))
	      (otherwise absolute-path))
	    (make-pathname :directory relative-pathspec))))))



(define-model medium ()
  ((file :handler #'handle-medium
	 :render-hint 'file :attribute t)
   (last-upload)
   (content-type :index t))
  (:visible nil))

(defgeneric handle-medium (object slot-name parameters)
  (:documentation "Handler for media objects (images, textfiles,
  videos etc.)  This method will copy the media file to the appropriate
  place in the file-system.  Media types that need something else
  done (thumbnail creation, transcoding etc) should create :BEFORE
  and :AFTER methods to do it."))

(defmethod handle-medium ((object medium) slot-name parameters)
  (let ((value (cdr (assoc slot-name parameters))))
    (when value
      (destructuring-bind (path file-name content-type)
	  value
	(let ((dest-path (make-pathname :directory (get-media-path object)
					:name (pathname-name file-name)
					:type (pathname-type file-name))))
	  (copy-file path dest-path)
	  (setf (get-file object) dest-path)
	  (setf (get-content-type object) content-type)
	  (setf (get-last-upload object) (get-universal-time)))))))
