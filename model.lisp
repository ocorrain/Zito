;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2009, Tiarnán Ó Corráin.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "ZITO")

(defvar *models* '() "A list of symbols donating class-names that correspond to models.")

(defclass model ()
  ((model-id :initarg :oid :initform (get-next-id) :reader get-model-id)
   (xrefs :initarg :xrefs :initform nil :accessor get-xrefs)
   (last-changed :initarg :last-changed :initform (get-universal-time)
		 :accessor get-last-changed)
   (published :initarg :published :initform nil :accessor get-published))
  (:metaclass persistent-metaclass))

(defun get-next-id ()
  (let ((current-id (or (get-from-root "id")
			0)))
    (add-to-root "id" (incf current-id))))

(defgeneric get-attributes (class-name)
  (:documentation "Returns the attributes (i.e. the slots that will
  show up in HTML forms and the like) for this object."))

(defgeneric handle-slot (object slot-name parameters)
  (:documentation "HANDLE-SLOT must handle what is needed from
  PARAMETERS and apply the necessary changes to SLOT-NAME in OBJECT.
  This function is responsible for validation as well as
  form-handling.

  It looks for the attribute :HANDLER for each slot and calls this
  function with OBJECT, SLOT-NAME and PARAMETERS.  If the function does not
  exist, it assumes that any value corresponding to SLOT-NAME in
  PARAMETERS is valid and will set the slot accordingly.  If there is
  no value for SLOT-NAME in PARAMETERS it will leave the value
  unchanged."))

(defgeneric render-slot (render-type slot-name slot-value stream))

(defmacro define-model (name superclasses slot-definitions)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (prog1
	 (defpclass ,name ,(if (some (lambda (super)
				       (subtypep super 'model)) superclasses)
			       superclasses
			       (cons 'model superclasses))
	   ,(mapcar (lambda (slot)
		      (destructuring-bind (slot-name . plist) slot
			`(,slot-name
			  :initarg ,(or (getf plist :initarg)
					(intern (string-upcase (symbol-name slot-name)) :keyword))
			  :initform ,(getf plist :initform)
			  :accessor ,(or (getf plist :accessor)
					 (intern (concatenate 'string "GET-" (symbol-name slot-name))))
			  ,@(when (getf plist :transient)
				  '(:transient t))
			  ,@(when (getf plist :index)
				  '(:index t)))))
		    slot-definitions)
	   (:index t))
       ,@(mappend (lambda (slot)
		    (destructuring-bind (slot-name . plist) slot
		      (when (getf plist :attribute)
			(list `(defmethod handle-slot ((object ,name) (slot-name (eql ',slot-name)) parameters)
				 ,(if (getf plist :handler)
				      `(funcall ,(getf plist :handler) object slot-name parameters)
				      `(let ((param-value (cdr (assoc slot-name parameters))))
					 (when param-value
					   (setf (slot-value object slot-name) param-value)))))
			      `(defmethod render-slot (render-type (slot-name (eql ',slot-name)) slot-value stream)
				 ,(let ((render-hint (getf plist :render-hint))
					(label (or (getf plist :label) (string-capitalize (symbol-name slot-name)))))
				       (if render-hint
					   `(render-with-hint render-type ,render-hint ,label slot-name slot-value stream)
					   `(render-with-hint render-type 'text ,label slot-name slot-value stream))))))))
		  slot-definitions)
       ,(let ((attrib-slots nil))
	     ;; first get the attributes for this class
	     (dolist (s slot-definitions)
	       (destructuring-bind (slot-name . plist) s
		 (when (getf plist :attribute)
		   (push slot-name attrib-slots))))
	     `(defmethod get-attributes ((class-name (eql ',name)))
		(remove-duplicates
		 (append ',(reverse attrib-slots)
			 (mappend #'get-attributes
				  (remove-if (lambda (class-name)
					       (or (eq class-name 'model)
						   (not (subtypep class-name 'model))))
					     (mapcar #'class-name (cdr (class-precedence-list (find-class class-name))))))))))
       (pushnew ',name *models*))))
 



(defgeneric render-with-hint (render-type hint label slot-name slot-value stream))
