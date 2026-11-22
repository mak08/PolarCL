(defpackage :polarcl-charset-tests
  (:use :cl)
  (:export :run-tests))
(in-package :polarcl-charset-tests)

(defclass input ()
  ((bytes :initarg :bytes :accessor bytes)
   (lines :initarg :lines :initform nil :accessor lines)))
(defmethod mbedtls:get-octets ((input input) length)
  (let ((end (or length (length (bytes input)))))
    (prog1 (subseq (bytes input) 0 end)
      (setf (bytes input) (subseq (bytes input) end)))))
(defmethod mbedtls:get-line ((input input) &key timeout)
  (declare (ignore timeout))
  (pop (lines input)))

(defun decode-body (payload content-type &key gzip chunked)
  (let* ((wire (if gzip (zlib:gzip-bytes payload) payload))
         (request (polarcl::make-http-post))
         (input (make-instance 'input :bytes wire)))
    (when content-type
      (setf (polarcl:http-header request :|Content-Type|) content-type))
    (if chunked
        (progn
          (setf (polarcl:http-header request :|Transfer-Encoding|) "chunked")
          ;; One byte per chunk: deliberately split multibyte characters.
          (setf (bytes input)
                (coerce (loop for byte across wire append (list byte 13 10))
                        '(vector (unsigned-byte 8)))
                (lines input) (append (loop repeat (length wire) collect "1") '("0"))))
        (setf (polarcl:http-header request :|Content-Length|)
              (write-to-string (length wire))))
    (when gzip
      (setf (polarcl:http-header request :|Content-Encoding|) "gzip"))
    (polarcl::get-body input request)
    (polarcl:http-body request)))

(defun run-tests ()
  (let ((utf8 (babel:string-to-octets "Grüße € 😀" :encoding :utf-8))
        (latin1 (babel:string-to-octets "Grüße" :encoding :iso-8859-1)))
    (dolist (type '("text/plain" "text/plain; format=flowed"
                    "text/plain; charset=utf-8" "text/plain; CHARSET=\"UTF-8\""))
      (assert (equal "Grüße € 😀" (decode-body utf8 type))))
    (dolist (type (list "text/plain; charset=iso-8859-1"
                       "text/plain; ChArSeT = \"ISO-8859-1\""
                       (format nil "text/plain;~cCHARSET~c=~ciso-8859-1~c"
                               #\Tab #\Tab #\Tab #\Tab)))
      (assert (equal "Grüße" (decode-body latin1 type))))
    (assert (eq :utf-8 (polarcl::http-charset (polarcl:make-http-get))))
    (assert (equal "Grüße" (decode-body latin1 "text/plain; charset=iso-8859-1" :gzip t)))
    (assert (equal "Grüße € 😀" (decode-body utf8 "text/plain" :chunked t)))
    (assert (equalp latin1 (decode-body latin1 "application/octet-stream")))
    (assert (handler-case
                (progn (decode-body latin1 "text/plain; charset=unknown-test-charset") nil)
              (error () t)))
    ;; Removing the slot must reject the obsolete initialization argument.
    (assert (handler-case
                (progn (make-instance 'polarcl:http-server :default-charset "latin1") nil)
              (error () t))))
  (format t "~&PolarCL request charset tests passed.~%")
  t)
