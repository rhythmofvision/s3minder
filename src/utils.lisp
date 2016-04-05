
(in-package :s3minder-util)

;;;; from key.lisp

;; (defun empty (thing)
;;   "Check if thing is not empty"
;;   (eq (length thing) 0))

(defun last-char (str)
  "Get strings last character" 
  (char str (1- (length str))))

(defun last-char-slash-p (str)
  (eq (last-char str) #\/))

(defun remove-trailing-slash (str)
  (when str 
    (if (eql (last-char str) #\/)
        (subseq str 0 (1- (length str)))
        str)))

(defun mapnlst (fn lst)
  ""
  (let ((res '())
        (len (length lst)))
    (do ((i 1 (1+ i)))
        ((> i len) (nreverse res))
      (push (funcall fn (subseq lst 0 i)) res))))

(defun mapplist (fn plist)
  "Map plist, call fn with key and value, fn should return two values,
will contruct new plist"
  (let ((res (list)))
    (alexandria:doplist (k v plist res)
      (multiple-value-bind (res-k res-v)
          (funcall fn k v)
        (setf (getf res res-k) res-v)))))

(defun to-string (symbol)
  (string-downcase (symbol-name symbol)))

(defun meta-json (data)
  (write-json-to-string
   (apply #'jso
    (mapplist #'(lambda (name value)
                  (values (to-string name) value))
              data))))

(defun json-meta-to-plist (data)
  "Turn json meta data into a plist"
  (let ((res '()))
    (labels ((jso-to-plist (obj)
               (mapjso #'(lambda (key val)
                           (push (intern (string-upcase key) "KEYWORD") res)
                           (push val res))
                       obj)))
      (jso-to-plist (read-json data))
      (nreverse res))))

(defun alist-meta-json (alist)
  "Turn meta encoded as alist into json string"
  (meta-json alist))

(defun join-prefix (string-list &key (trailing t))
  "Concatenates a list of strings and puts / symbol between the
elements."
  (let ((format-string (if trailing
                           "~{~A~^/~}/"
                           "~{~A~^/~}")))
    (if (> (length string-list) 0)
        (format nil format-string string-list)
        "")))

(defun except-last (lst)
  "Return subsequence of list except the last element"
  (subseq lst 0 (- (length lst) 1)))

(defun string-diff (str-a str-b)
  "Return string difference"
  (let ((contains (search str-a str-b)))
    (when (eq contains 0)
      (subseq str-b (length str-a)))))

;;;; s3m specific

(defmacro concat (&rest args)
  `(concatenate 'string ,@args))

(defun build-lookup-hash (list &key (test 'eql) (value t))
  "Converts list to hash with keys equals to each element of list and
  value equalls to value (default is t)"
  (let ((table (make-hash-table :test test)))
    (mapcar #'(lambda (key)
                (setf (gethash key table) value))
            list)
    table))
    
(defun get-keys (params &optional (key-name "keys[]"))
  (iter (for (name . val) in params)
        (when (string= name key-name)
          (collect val))))

(defun sign (key data)
  (let* ((data-utf8 (or (and (stringp data) (string-octets data))
                        data))
         (hmac (ironclad:make-hmac key :sha256)))
    (ironclad:update-hmac hmac data-utf8)
    (ironclad:hmac-digest hmac)))

(defun get-signature-key (date secret-key &key (region "us-east-1") (service "s3"))
  (let* ((date-key (sign (string-octets (concatenate 'string "AWS4" secret-key))
                         date))
         (date-region-key (sign date-key region))
         (date-region-service-key (sign date-region-key service))
         (signing-key (sign date-region-service-key "aws4_request")))
    signing-key))

(defun string-octets (string)
  "Return the UTF-8 encoding of STRING as a vector of octets."
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun string64 (string)
  (cl-base64:usb8-array-to-base64-string string))

(defun hash-string (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1 (s3minder-web::string-octets string))))

(defun get-policy (bucket prefix date)
  (write-json-to-string
   (jso "expiration" (policy-expire-date)
        "conditions" (list (jso "bucket" bucket)
                           (list "starts-with" "$key" (or prefix ""))
                           (jso "x-amz-credential" "")
                           (jso "x-amz-algorithm" "AWS4-HMAC-SHA256")
                           (jso "x-amz-date" date)))))

(defun policy-expire-date ()
  "2015-03-30T12:00:00.000Z")

(defun policy-date ()
  "20150330T120000Z")
