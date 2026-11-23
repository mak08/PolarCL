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

## Tests

Run `make` in the PolarCL directory first to generate `ctypes.cl` if it is absent.
With PolarCL and its dependencies available to ASDF:

```lisp
(asdf:test-system "polarcl")
;; Or run the test system directly:
(asdf:test-system "polarcl/tests")
```

Both commands run the configuration/authorization, HTTP response and request
charset and PUT regression suites, including authentication protocols, HEAD, UTF-8 lengths and gzip. Tests use captured output rather
than network listeners and isolate configuration registries. Assertion failures
signal errors. Loading `polarcl` alone does not load or run the tests.

## Description
PolarCL is a web server based on [mbedTLS](https://www.mbed.com/en/technologies/security/mbed-tls/) (formerly known as PolarSSL). It is still work in progress.
PolarCL currently supports
* Secure (TLS) and plain socket connections
* Multithreading using a thread pool or on-demand thread creation
* Connection keep-alive
* Explicit per-route authentication: public, Basic, Bearer, or Digest SHA-256 (`qop=auth`)
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


## Authentication

Each `handle` requires `:authentication nil` or a scheme policy. See the
[authentication API](Manual.md#authentication) for local credential stores,
provider hooks, TLS requirements and migration from the old authorizer API.
Authorization, SSO and identity-provider workflows remain application concerns.
The built-in stores require mbedTLS 3 (PBKDF2), SBCL and `/dev/urandom`.

## Limitations

PUT bodies are read and available to application handlers; resource creation or
replacement must be implemented by the application. See the manual for current
limitations, including experimental WebSocket support.
POST requests with an explicit `Content-Length: 0` are handled by the current code.
