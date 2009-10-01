(in-package "ZITO")

(define-model text (medium)
  ((contents :render-hint 'textarea :attribute t)
   (contents-last-uploaded)
   (date :attribute t)))

(define-model monograph (text)
  ((author :index t :attribute t)
   (publisher :index t :attribute t)
   (place :index t :attribute t)
   (editor :index t :attribute t)))

(define-model journal-article (monograph)
  ((journal :index t :attribute t)))

(define-model journal ()
  ((title :attribute t)
   (volume :handler #'number-from-string :attribute t)
   (number :handler #'number-from-string :attribute t)
   (editor :attribute t)
   (publisher :attribute t)
   (place :attribute t)
   (articles :render-hint #'choose-articles :attribute t)))

(defmethod handle-medium :after ((object text) slot-name parameters)
  (when (> (get-last-upload object)
	   (get-contents-last-uploaded object))
    (process-text-input object)))

