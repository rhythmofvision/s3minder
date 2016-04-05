
(in-package :s3minder-web)

;; check if user owns keys

(defclass owns-keys (routes:proxy-route)
  ((param-name :initarg :param-name
               :accessor key-param-name))
  (:default-initargs
   :param-name "keys[]"))

(defmethod routes:route-check-conditions ((route owns-keys) bindings)
  (let ((owns (user-owns-keys (session-user) (get-keys (hunchentoot:post-parameters*)
                                                       (key-param-name route)))))
                                        ;(when (not owns)
    (setf (hunchentoot:return-code hunchentoot:*reply*) 400)
    (hunchentoot:abort-request-handler)))

(defun @owns-keys (route)
  (make-instance 'owns-keys :target route))

;; auth

(defclass requires-auth (routes:proxy-route) ())

(defmethod routes:route-check-conditions ((route requires-auth) bindings)
  (and (call-next-method)
       (if (not (validate-auth))
           (restas:redirect 's3minder-pages::login)
           t)))

(defun @requires-auth (route)
  (make-instance 'requires-auth :target route))

(defun clear-session ()
  (setf (hts:session-value :user) nil
        (hts:session-value :user-email) nil))

(defun create-session (user-dao)
  (setf (hts:session-value :user) (user-id user-dao)
        (hts:session-value :user-email) (email user-dao)))

(defun validate-auth ()
  (let ((user-id (hts:session-value :user)))
    (when (or (and (integerp user-id) (plusp user-id))
              (plusp (length user-id)))
      (if (integerp user-id)
          user-id
          (parse-integer user-id)))))

(defun session-user ()
  (alexandria:when-let ((user-id (validate-auth)))
    (find-user-by-id user-id)))
