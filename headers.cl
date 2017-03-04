;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2016
;;; Last Modified <michael 2017-02-24 00:14:00>

(in-package "POLARCL")

(defparameter *known-request-headers*
  (list :|Accept|
        :|Accept-Charset|
        :|Accept-Encoding|
        :|Accept-Language|
        ;; :|Authorization|
        :|Cache-Control|
        :|Connection|
        :|Cookie|
        :|Content-Length|
        ;; :|Content-MD5 |
        :|Content-Type|
        :|Date|
        ;; :|Expect|
        ;; :|From|
        :|Host|
        :|If-Match|
        :|If-Modified-Since|
        :|If-None-Match|
        :|If-Range|
        :|If-Unmodified-Since|
        ;; :|Max-Forwards|
        ;; :|Pragma|
        ;; :|Proxy-Authorization|
        ;; :|Range|
        ;; :|Referer[sic]|
        ;; :|TE|
        :|Transfer-Encoding|
        ;; :|Upgrade|
        :|User-Agent|
        :|Via|
        ;; :|Warning|
        ))
(defparameter *known-response-headers*
  (list ;; :|Accept-Ranges|
        :|Age|
        ;; :|Allow|
        :|Cache-Control|
        :|Connection |
        :|Content-Encoding|
        ;; :|Content-Language|
        :|Content-Length|
        ;; :|Content-Location|
        ;; :|Content-MD5|
        :|Content-Disposition|
        ;; :|Content-Range|
        ;; :|Content-Security-Policy|
        :|Content-Type|
        :|Date|
        :|ETag|
        :|Expires|
        :|Last-Modified|
        ;; :|Link|
        ;; :|Location|
        ;; :|P3P|
        ;; :|Pragma|
        ;; :|Proxy-Authenticate|
        ;; :|Refresh|
        ;; :|Retry-After|
        :|Server|
        :|Set-Cookie|
        ;; :|Trailer|
        :|Transfer-Encoding|
        ;; :|Vary|
        :|Via|
        :|Warning|
        ;; :|WWW-Authenticate|
        ))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
