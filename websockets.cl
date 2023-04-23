;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2023-01-29 02:52:10>

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Websocket handler

(defclass websocket-handler (handler)
  ((wsfunction :accessor wsfunction :initarg :wsfunction)))

(defmethod handle-response ((http-server http-server) (filter t) (handler websocket-handler) (connection t) (request t) (response t))
  (let ((connection-header (http-header request :connection))
        (sec-ws-key-header (http-header request "sec-websocket-key")))
    (log2:info "~a ~a" connection-header sec-ws-key-header)
    (log2:info "Response: ~a" response)
    (setf (status-code response) "101")
    (setf (status-text response) "Switching Protocols")
    (setf (http-header response "Connection") "Upgrade")
    (setf (http-header response "Upgrade") "websocket")
    (setf (http-header response "Sec-WebSocket-Accept")
          (base64:usb8-array-to-base64-string (mbedtls:mbedtls-md  (concatenate 'string sec-ws-key-header "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") :method "SHA1" :result-type :bytes)))
    (write-response connection response)
    (funcall (wsfunction handler) http-server handler request response)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
