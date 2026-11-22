(defpackage :polarcl-put-tests
  (:use :cl)
  (:export :run-tests))
(in-package :polarcl-put-tests)

;; Headers and bodies share one byte stream, so unread PUT bytes would corrupt
;; the following request exactly as on a persistent connection.
(defclass input ()
  ((bytes :initarg :bytes :reader bytes)
   (offset :initform 0 :accessor offset)))
(defmethod mbedtls:get-octets ((input input) count)
  (let ((end (if count (+ (offset input) count) (length (bytes input)))))
    (prog1 (subseq (bytes input) (offset input) end)
      (setf (offset input) end))))
(defmethod mbedtls:get-line ((input input) &key timeout)
  (declare (ignore timeout))
  (let ((end (search #(13 10) (bytes input) :start2 (offset input))))
    (assert end)
    (prog1 (babel:octets-to-string (subseq (bytes input) (offset input) end))
      (setf (offset input) (+ end 2)))))

(defun header-bytes (&rest lines)
  (babel:string-to-octets
   (format nil "~{~a~c~c~}" (loop for line in lines append (list line #\Return #\Newline)))))

(defun run-tests ()
  (dolist (method '("PUT" "POST"))
    (dolist (case '(("Grüße 😀" :utf-8 "text/plain" nil)
                    ("Grüße" :iso-8859-1 "text/plain; charset=iso-8859-1" nil)
                    ("Grüße" :utf-8 "text/plain; charset=utf-8" t)
                    ("" :utf-8 "text/plain" nil)
                    (nil nil "application/octet-stream" nil)))
      (destructuring-bind (text encoding content-type gzip) case
        (let* ((payload (if text (babel:string-to-octets text :encoding encoding)
                            (make-array 4 :element-type '(unsigned-byte 8)
                                          :initial-contents '(0 255 128 10))))
               (wire (if gzip (zlib:gzip-bytes payload) payload))
               (input (make-instance 'input :bytes
                        (concatenate '(vector (unsigned-byte 8))
                         (apply #'header-bytes
                          (append (list (format nil "~a /resource HTTP/1.1" method)
                                        "Host: localhost" "Connection: keep-alive"
                                        (format nil "Content-Type: ~a" content-type)
                                        (format nil "Content-Length: ~d" (length wire)))
                                  (when gzip '("Content-Encoding: gzip")) '("")))
                         wire
                         (header-bytes "GET /next HTTP/1.1" "Host: localhost" ""))))
               (request (polarcl::create-request input "8080" (mbedtls:get-line input))))
          (assert (typep request (if (string= method "PUT")
                                    'polarcl::http-put 'polarcl::http-post)))
          (polarcl::read-request input request)
          (assert (equalp (or text payload) (polarcl:http-body request)))
          ;; Use the real dynamic handler callback path to inspect the body.
          (let ((response (polarcl::make-ok-response request)))
            (polarcl::handle-response
             (make-instance 'polarcl:http-server) nil
             (polarcl:create-handler 'polarcl:dynhtml-handler
              :contentfn (lambda (server handler req res)
                           (declare (ignore server handler))
                           (setf (polarcl:http-body res) (polarcl:http-body req))))
             input request response)
            (assert (equalp (or text payload) (polarcl:http-body response))))
          (let ((next (polarcl::create-request input "8080" (mbedtls:get-line input))))
            (polarcl::read-request input next)
            (assert (typep next 'polarcl:http-get))
            (assert (equal '("next") (polarcl:path next))))
          (assert (= (offset input) (length (bytes input))))))))
  (format t "~&PolarCL PUT request tests passed.~%")
  t)
