;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael 2014
;;; Last Modified <michael 2017-02-24 00:14:57>

(in-package "POLARCL")

(defun read-octet-stream (s)
  (do* ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (count 1024)
        (size 0)
        (result (make-array 0 :element-type '(unsigned-byte 8) :adjustable t)))
       ((< count 1024) result)
    (setf count (read-sequence buffer s))
    (adjust-array result (+ size count))
    (setf (subseq result size (incf size count))
          (subseq buffer 0 count))))

(defun read-octets-from-stream (stream count)
  (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    buffer))

(defun read-octet-stream-line (s)
  (do* ((result (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
        (byte (read-byte s nil nil)
              (read-byte s nil nil)))
       ((or (null byte)
            (and (= byte 13)
                 (let ((next-byte (read-byte s nil nil)))
                   (or (= next-byte 10)
                       (null next-byte)
                       (error "Unexpected or missing line termination")))))
        (values result byte))
    (vector-push-extend byte result)))

(defun read-octet-stream-lines (s)
  (do ((line nil)
       (more t)
       (result ()))
      ((not more) (nreverse result))
    (multiple-value-setq (line more)
      (read-octet-stream-line s))
    (push line result)))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


