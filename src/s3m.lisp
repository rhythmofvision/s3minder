
(in-package :s3minder)

(defmacro concat (&rest args)
  `(concatenate 'string ,@args))

(defun str-ends-with (ending str &key (test #'equal))
  (let ((len (length str)))
    (funcall test (subseq str (1- len) len) ending)))

(defun check-aws-credentials (access-key secret-key)
  (handler-case
      (zs3:me :credentials (list access-key secret-key))
    (zs3::request-error (c)
      (declare (ignore c))
      nil)))
