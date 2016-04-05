
(in-package :s3minder)

;; soon to be full featured testing for the project and right now only
;; a couple of test routines executed manually

(defun auth-user-cookie-jar (&optional (email "mikhail.malyuk@protonmail.com"))
  (let ((session (make-instance 'hunchentoot-sessions::session)))
    (setf (hunchentoot-sessions:session-value :user session) "2"
          (hunchentoot-sessions:session-value :user-email session) email)
    (hunchentoot-sessions::save-session session)
    (let ((cookie (make-instance 'drakma::cookie
                                 :name "hts"
                                 :domain "127.0.0.1"
                                 :value (hunchentoot-sessions::session-id session))))
      (make-instance 'drakma:cookie-jar
                     :cookies (list cookie)))))

(defun test-delete-key-api (user)
  (let ((keys (build-test-buckets user)))
    (drakma:http-request "http://127.0.0.1:9615/" 
                         :method :post
                         :parameters (list (cons "keys[]" (car keys)))
                         :cookie-jar (auth-user-cookie-jar))
    (let ((zs3:*credentials* (s3minder-web::s3-creds user)))
      (assert (not (exists-s3-p (parse-key (car keys))))))))

(defun test-move-key-api ()
  )

(defun test-copy-key-api ()
  )

(defun build-test-buckets (user &optional (bucket-prefix "test-bucket-cHaF"))
  "Build test buckets and upload a number of files in there, this will
  test upload functions as well as providing a test data for further
  testing"
  (let ((keys '()))
    (dolist (i (list 1 2 3))
      (let* ((idx (write-to-string i))
             (bucket-name (concat bucket-prefix "-" idx))
             (test-key (concat "test" idx "/subpath" idx "/file" idx ".txt"))
             (test-data (concat "data" idx)))
        (zs3:create-bucket bucket-name :credentials (s3minder-web::s3-creds user))
        (zs3:put-string test-data bucket-name test-key :credentials (s3minder-web::s3-creds user))
        (pushnew (concat "s3://" bucket-name "/" test-key) keys)
        (s3minder-web::sync-bucket user bucket-name)))
    keys))

(defun path-key-meta-key (key)
  "From path key construct associated key to fetch meta data"
  (let ((len (length key)))
    (if (string= "/" (subseq key (- len 1) len))
        (subseq key 0 (- len 1))
        key)))

(defun test-key-meta (key)
  "Test if pkey has metadata"
  (assert (> (length (red:get key)) 0)))

(defun test-redis-consistency ()
  "Check if whatever we have in redis is consistent, this function
  sort of defines the latest format used for storage"

  (redis:with-connection ()
    (let ((keys (red:keys "*")))
      (dolist (key keys)
        (let ((pkey (parse-key key)))
          ;; means we deal with path
          (when (and (eq 0 (length (key-name pkey)))
                     (> (length (key-path pkey)) 0))
            ;; each path should have an associated key containing path
            ;; meta data
            (test-key-meta (path-key-meta-key key))
            
            ;; each path key may have something inside, and something
            ;; that is inside should have associated meta data keys as
            ;; well
            (dolist (child (red:lrange key 0 5000))
              (test-key-meta (concat key child)))))))))
            
