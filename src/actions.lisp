
(in-package :s3minder)

(defparameter *search-results-num* 50)

(defun filter-s3-responses (responses &key (status 200))
   (iter (for (key . res) in responses)
         (when (eq (zs3::http-code res) status)
           (collect key))))

(defun check-keys-format (params)
  "Structure of key is $bucket:/$path/$file where there might be more
than one $path or no $path/$file at all,

this are all valid examples of keys:
s3minder:/
s3minder:/some/path/
s3minder:/some/path/file.jpg
s3minder:/file.jpg"
  (let* ((keys (iter (for (name . val) in params)
                     (when (string= name "keys[]")
                       (collect val))))
         
         (num (length keys)))
    (and (> num 0)
         (= (length (remove-if-not (lambda (key) (find #\: key)) keys))
            num))))

(defun change-scheme (key new-scheme)
  (concat new-scheme (subseq key (position #\: key))))

;; Helper functions which connects web API and key API

(defun prepare-keys (keys dest)
  (let ((dest-key (parse-key dest))
        (keys-keys (mapcar #'parse-key keys)))
    (fetch-keys-meta keys-keys)
    (fetch-keys-meta (list dest-key))
    (values keys-keys dest-key)))
    

(defun copy-keys (user dest-str keys-str)
  (redis:with-connection ()
    (multiple-value-bind (keys dest)
        (prepare-keys keys-str dest-str)
      (let ((zs3:*credentials* (s3minder-web::s3-creds user)))
        (mapcar (curry #'copy-key dest) keys)))))


(defun move-keys (user dest-str keys-str)
  (redis:with-connection ()
    (multiple-value-bind (keys dest)
        (prepare-keys keys-str dest-str)
      (let ((zs3:*credentials* (s3minder-web::s3-creds user)))
        (mapcar (curry #'copy-key dest) keys)
        (mapcar #'delete-key keys)))))


(defun delete-keys (user keys-str)
  (let ((keys (mapcar #'parse-key keys-str)))
    (redis:with-connection ()
      (fetch-keys-meta keys)
      (let ((zs3:*credentials* (s3minder-web::s3-creds user)))
        (mapcar #'delete-key keys)))))


(defun search-keys (query)
  (redis:with-connection ()
               ;; (jso "key" name
               ;;      "size" 123456
               ;;      "kind" "image"
               ;;      "bucket" bucket
               ;;      "sc" 0
               ;;      "owner" "mikhail.malyuk@protonmail.com"
               ;;      "lm" "date"
               ;;      "meta" "1920x1294")
    (let ((keys (red:keys (concat "s3://*" query "*"))))
      (when (> (length keys) 50)
        (setf keys (subseq keys 0 *search-results-num*)))
      (fetch-keys-meta (mapcar #'parse-key keys)))))
