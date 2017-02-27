# PolarSeal
A web server based on mbedTLS

## Features
PolarSeal is a web server based on [mbedTLS](https://tls.mbed.org/) (formerly known as PolarSSL). It still work in progress.
PolarSeal currently supports
* Secure (TLS) and plain socket connections
* Multithreading using a thread pool or on-demand thread creation
* Connection keep-alive
* Basic authentication (Digest access authentication is planned)
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

Furthermore, Polar Seal supports server-side programming in Common Lisp 
* Registering handlers for serving static and dynamic content
* Easy registration of Lisp functions to be called from XHR GET requests, for example.
