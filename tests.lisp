(in-package "ZITO")



(define-model portrait (captioned-image)
  ((artist :render-hint 'text :index t :attribute t)
   (subject :render-hint 'text :attribute t)
   ;; (date :render-hint #'make-date-widget
   ;; 	 :validator #'validate-date :index t :attribute t)
   (materials :render-hint 'text :attribute t)
   (dimensions :render-hint 'text :attribute t)))

(define-model gallery ()
  ((title :render-hint 'text :attribute t)
   (images)))

(define-model gmap ()
  ((title :render-hint 'text :attribute t :index t)
   (caption :render-hint 'textarea :attribute t)
   (coordinates :render-hint '((text longitude) (text latitude))
		:validator #'validate-longitude/latitude :attribute t)
   (magnification :render-hint text :attribute t)
   (options :render-hint option-list :initform *default-map-options*)))


 