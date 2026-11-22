# PolarCL
A web server based on mbedTLS

## Documentation

See the [external API and configuration manual](Manual.md) for server options,
HTTP/HTTPS setup, routing, authentication, request/response APIs, cookies and
implementation limitations. The [runnable example configuration](examples/api-config.cl)
demonstrates dynamic content, registered functions, POST bodies, static files and
redirects, with corresponding curl commands in the manual.

```lisp
(ql:quickload "polarcl")
(polarcl:load-configuration "/absolute/path/to/PolarCL/examples/api-config.cl")
;; Stop when finished:
(polarcl:stop-all-servers)
```

## Description
PolarCL is a web server based on [mbedTLS](https://www.mbed.com/en/technologies/security/mbed-tls/) (formerly known as PolarSSL). It is still work in progress.
PolarCL currently supports
* Secure (TLS) and plain socket connections
* Multithreading using a thread pool or on-demand thread creation
* Connection keep-alive
* Basic authentication (Digest access authentication is planned)
* Cookies
* Redirection
* Conditional requests (if-modified-since)
* UTF-8
* Chunked transfer, Compressed encoding (gzip)
* Methods GET, POST, PUT, OPTIONS
* Content types 
  * application/octet-stream,image/png
  * text/
  * application/xml,application/x-www-form-urlencoded
* Logging

Furthermore, PolarCL supports server-side programming in Common Lisp 
* Registering handlers for serving static and dynamic content
* Easy registration of Lisp functions to be called from XHR GET requests, for example.

## Depends on
* Quicklisp libraries
   * bordeaux-threads
   * puri
   * cl-base64
   * cffi
   * local-time
* Github projects
   * cl-mbedtls
   * makros
   * zlib

The system also declares cl-ppcre; see [polarcl.asd](polarcl.asd) and the manual
for dependency and native-library setup details.


## Limitations

See the manual for current limitations, including PUT body handling and
experimental WebSocket support.
POST requests with an explicit `Content-Length: 0` are handled by the current code.
