(in-package #:gbbopen-user)




;;; ====================================================================
;;;   Location, our only unit-instance class.

(define-unit-class location ()
  (time
   x
   y
   (next-location :link (location previous-location :singular t)
                  :singular t)
   (previous-location :link (location next-location :singular t)
                  :singular t))
  (:dimensional-values
   (time :point time)
   (x :point x)
   (y :point y))
  (:initial-space-instances (known-world)))

(defmethod print-instance-slots ((location location) stream)
  (call-next-method)
  (when (and (slot-boundp location 'x)
             (slot-boundp location 'y))
    (format stream " (~s ~s)"
            (x-of location)
            (y-of location))))




;;; ====================================================================
;;;   Startup KS

(macrolet ((define-my-ks (name (ksa &rest keys) &body body)
             (let ((execution-function (alexandria:symbolicate 'execution-function-of- name)))
               `(progn
                 (defun ,execution-function (,ksa) ,@body)
                 (define-ks ,name 
                     :execution-function #',execution-function
                     ,@keys)))))

  (define-my-ks startup (ksa :trigger-events '((control-shell-started-event)))
    (declare (ignore ksa))
    (make-instance 'location :instance-name 'start :time 0 :x 0 :y 0)))

;; (find-ks-by-name 'startup-ks)
;; (map-instances-of-class #'print 'ks)


;; (start-control-shell)
;; (start-control-shell :stepping t)




;;; ====================================================================
;;;   Startup KS

(macrolet ((define-my-ks (name (ksa &rest keys) &body body)
             (let ((execution-function (alexandria:symbolicate 'execution-function-of- name)))
               `(progn
                  (defun ,execution-function (,ksa) ,@body)
                  (define-ks ,name
                      :execution-function #',execution-function
                      ,@keys)))))

  (define-my-ks random-walker (ksa :rating 100 
                                   :trigger-events '((instance-created-event location)))
    (declare (ignore ksa))
    (let* ((stop-after 15)
           (old-location (sole-trigger-instance-of ksa))
           (time (time-of old-location)))
      (cond
        ((>= time stop-after) ;; That's enough of that!
         (format t "~2&Finished walk, exausted.~%"))
        (t ;; The new location is +/- 10 of the stimulus's location:
         (flet ((f (x)
                  (let ((new-x (+ x (random 21) -10)))
                    (cond
                      ((< 50 new-x) 50)
                      ((< new-x -50) -50)
                      (t new-x)))))
           (make-instance 'location 
                          :time (1+ time)
                          :x (f (x-of old-location))
                          :y (f (y-of old-location))
                          :previous-location old-location)))))))

;;; ====================================================================
;;;   on Control-Shell-Started-Event

(macrolet ((define-initialization (name (priority) &body body)
             `(progn
                (defun ,name (event-name &key &allow-other-keys)
                  (declare (ignore event-name))
                  ,@body)
                (add-event-function
                 ',name 'control-shell-started-event :priority ,priority))))

  (define-initialization create-the-world (100)
    (let* ((classes '(location))
           (dimensions (loop for c in classes append (dimensions-of c)))
           (space-name
            (first
             (slot-value (find-class (first classes))
                         'gbbopen::initial-space-instances))))
      (make-space-instance space-name
                           :allowed-unit-classes classes
                           :dimensions dimensions))))




;;; ====================================================================
;;;   on Quiescence

(flet ((print-walk ()
         (loop
            initially (format t "~&Journal of our travels:")
            for loc? = (find-instance-by-name 'start 'location)
            then (next-location-of loc?)
            while loc?
              do (with-slots ((name instance-name) x y) loc?
                   (format t "~&  ~5A: ~3D, ~3D" name x y)))))
  (define-ks quiesence-ks
    :trigger-events ((quiescence-event))
    :rating 100
    :execution-function #'(lambda (ksa)
                            (declare (ignore ksa))
                            (print-walk)
                            ;; Tell the Agenda Shell to exit:
                            :stop)))




;;; ====================================================================
;;; go go!

(defun go-go ()
  (delete-blackboard-repository)
  (check-link-definitions t t)
  (disable-event-printing)
  ;; (enable-event-printing '(create/delete-instance-event :plus-subevents))
  ;; (enable-event-printing '(control-shell-event :plus-subevents))
  ;; (enable-event-printing '(single-instance-event :plus-subevents))
  ;; (enable-event-printing '(single-instance-event :plus-subevents) 'location)
  (start-control-shell :print nil)
  ;; (describe-blackboard-repository)
  )

;; (enable-event-printing)
;; (disable-event-printing)
;; (map nil #'print (sort (find-instances-of-class 'location) #'< :key #'time-of))
;; (map-sorted-instances-of-class #'print 'location #'< :key #'time-of)
