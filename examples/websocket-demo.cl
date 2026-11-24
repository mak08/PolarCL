;;; Load PolarCL, then this file. Start your HTTP/HTTPS server separately.
;;; For an HTTP server at localhost:8080, open a page from that same origin:
;;; const socket = new WebSocket("ws://localhost:8080/events");
;;; socket.onmessage = event => console.log(JSON.parse(event.data));
;;; socket.onopen = () => socket.send(JSON.stringify({message: "Hello", enabled: true}));
(in-package :cl-user)

(defun websocket-demo-open (socket)
  (setf (polarcl:websocket-data socket) 0))

(defun websocket-demo-message (socket value)
  (incf (polarcl:websocket-data socket))
  (polarcl:websocket-send socket value))

(defun websocket-demo-close (socket code reason clean-p)
  (format t "~&WebSocket closed after ~d messages: ~d ~a (clean: ~s)~%"
          (polarcl:websocket-data socket) code reason clean-p))

(polarcl:handle
 :request (:path "/events")
 :handler (:websocket (:format :json
                      :origins ("http://localhost:8080")
                      :on-open #'websocket-demo-open
                      :on-message #'websocket-demo-message
                      :on-close #'websocket-demo-close)
           :authentication nil))
