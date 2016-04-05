
(in-package :s3minder-api)

; (defvar *test* nil)

; (defparameter *s3-sec* "YwQcUFwdWmuT7I4QSBJfEXVQy6zZC50H9x4sgcvE")

;(defparameter *s3-sec* "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")

;; (defsection @route-test (:title "Hello world")
;;   (s3-upload-policy rroute))

(defsection @api-manual (:title "API Manual")
  "API Manual"
  (s3-upload-policy function)
  (@keys-api section))

(defsection @keys-api (:title "Keys management api")
  "Manage keys"
  (add-key function)
  (copy-keys function)
  (move-keys function)
  (delete-keys function))

(defun @check-keys ()
  (s3minder::check-keys-format (ht:post-parameters*)))

(defun @check-keys-and-dest ()
  (and (s3minder::check-keys-format (ht:post-parameters*))
       (stringp (ht:post-parameter "dest"))
       (plusp (length (ht:post-parameter "dest")))))

(defun gen-s3-policy (s3-secret key)
  (let* ((pkey (s3minder::parse-key key))
         (date (policy-date))
         (policy-utf8 (string-octets (get-policy (s3minder::key-bucket pkey) (s3minder::key-prefix pkey) date)))
         (policy-base64 (string64 policy-utf8))
         (signature-key (get-signature-key "20150330" s3-secret))
         (signature (sign signature-key policy-base64))
         (signature-hex (ironclad:byte-array-to-hex-string signature)))
    (values signature-hex policy-base64 date)))

(restas:define-route finalize-upload ("/upload/finalize" :method :post)
  ""
  )

(restas:define-route s3-upload-policy ("/s3policy" :method :get)
  (:render-method #'write-json-to-string)
;  (:decorators #'@requires-auth)
  "Generate policy with 3a signature and date, expects the key"
  (setf (ht:content-type* ht:*reply*) "application/json")
  (multiple-value-bind (signature policy date)
      (gen-s3-policy (pw:s3-creds (pw:session-user)) (ht:get-parameter "key"))
    (jso "status" "okay"
         "date" date
         "signature" signature
         "policy" policy)))

(restas:define-route delete-keys ("/keys/del" :method :post)
  (:render-method #'write-json-to-string)
  ;; (:decorators #'pw:@owns-keys)
  ;; (:requirement #'@check-keys)
  "Delete keys, expects key(s)"
  (setf (ht:content-type* ht:*reply*) "application/json")
  (let ((res (s3minder::delete-keys (pw:session-user)
                                   (get-keys (ht:post-parameters*)))))
    ;; (if res
    ;;     (jso "status" "fail"
    ;;          "message" "failed")
        (jso "status" "okay")))

(restas:define-route move-keys ("/keys/move" :method :post)
  (:render-method #'write-json-to-string)
;  (:decorators #'@requires-auth)
  (:requirement #'@check-keys-and-dest)
  "Move keys, expects key(s) and dest"
  (setf (ht:content-type* ht:*reply*) "application/json")
  (let ((res (s3minder::move-keys (pw:session-user)
                                 (ht:post-parameter "dest")
                                 (get-keys (ht:post-parameters*)))))
    (jso "status" "okay"
         "response" res)))

(restas:define-route copy-keys ("/keys/copy" :method :post)
  (:render-method #'write-json-to-string)
 ; (:decorators #'pw:@requires-auth)
  (:requirement #'@check-keys-and-dest)
  "Copy keys, expects key(s) and dest"
  (setf (ht:content-type* ht:*reply*) "application/json")
  (let ((res (s3minder::copy-keys (pw:session-user)
                                 (ht:post-parameter "dest")
                                 (get-keys (ht:post-parameters*)))))
    (if res
        (jso "status" "fail"
             "message" "failed")
        (jso "status" "okay"))))

(restas:define-route add-key ("/keys" :method :post)
;  (:decorators #'@requires-auth)
  (:render-method #'write-json-to-string)
  "Post to add a key, expects a key and meta data"
  (let ((key (s3minder::parse-key (ht:post-parameter "key"))))
    (setf (s3minder::key-meta key)
          (s3minder::json-meta-to-plist (ht:post-parameter "meta")))
    (s3minder::save-key-redis key))
  ;; (s3minder::add-key (hunchentoot:post-parameter "key")
  ;;                   (hunchentoot:post-parameter "meta")
  (jso "status" "okay"))


(restas:define-route search-keys ("/search" :method :get)
;  (:decorators #'@requires-auth)
  (:render-method #'write-json-to-string)
  "Search the database, expects query"
  (let ((query (ht:get-parameter "query")))
    (jso "status" "okay"
         "keys" (s3minder::serialize-keys (s3minder::search-keys query)))))


(restas:define-route add-bucket ("/s3/buckets" :method :post)
;  (:decorators #'@requires-auth)
  (:requirement (lambda ()
                  (plusp (length (ht:post-parameter "name")))))
  (:render-method #'write-json-to-string)
  "Create new S3 bucket"
  (let ((key (s3minder::make-key (ht:post-parameter "name") "")))
    (s3minder::create-bkt key))
  ;; (s3minder::add-key (hunchentoot:post-parameter "key")
  ;;                   (hunchentoot:post-parameter "meta")
  (jso "status" "okay"))
