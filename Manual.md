# PolarCL external API and server configuration

This manual describes the implementation in this checkout. PolarCL is an
embedded Common Lisp HTTP/HTTPS server, not a client library or a predefined
REST service. Applications expose their own URLs through handlers. Public Lisp
symbols are exported by [package.cl](package.cl); names written with `polarcl::`
are implementation details, not exported API.

## Load and run

Make the `polarcl` ASDF system and its dependencies discoverable, then load it:

```lisp
(ql:quickload "polarcl")
(polarcl:load-configuration "/absolute/path/to/PolarCL/examples/api-config.cl")
```

The [runnable example](examples/api-config.cl) binds loopback port 8080 and
registers a health endpoint, a query function, a POST echo function, a static
file and a redirect. Keep the Lisp process running and try:

```sh
curl -i http://127.0.0.1:8080/health
# 200, text/plain, body: ok
curl -i 'http://127.0.0.1:8080/api/greet?name=Ada'
# body: Hello, Ada!
curl -i -H 'Content-Type: text/plain' --data 'hello' http://127.0.0.1:8080/api/echo
# body: hello
curl -i http://127.0.0.1:8080/
# 302, Location: HTTP://127.0.0.1:8080/index.html
curl -i http://127.0.0.1:8080/index.html
```

Stop all listeners with `(polarcl:stop-all-servers)`.

The system definition [polarcl.asd](polarcl.asd) declares bordeaux-threads, puri,
cl-base64, cffi, local-time, makros, cl-mbedtls, zlib and cl-ppcre. Source also
uses packages such as Babel, log2, cl-utilities and SB-POSIX supplied by the
local dependency/setup environment. Native mbedTLS and zlib libraries must be
available. Directory listings use SBCL's `sb-posix`. `ctypes.cl` is generated
from `ctypes.c` by the repository Makefile; it must exist when loading the system.

## Configuration lifecycle

`(load-configuration path)` loads **executable Lisp**, using `load`; it is not
an INI/JSON parser. Use trusted files and put an explicit `in-package` or
qualified names in them. Relative file names follow the Lisp pathname defaults,
not automatically the configuration directory. The runnable example uses
`*load-truename*` to locate its asset.

When the file exists, the loader requests that all servers stop, sleeps for
`1 + polarcl::*loop-delay*` seconds (normally three), resets configuration state,
and loads the file. Errors during this sequence are logged and swallowed;
a missing file signals an error before any server is stopped. Loading is not
transactional, so a failure can leave a partially configured application.

The internal `reset` clears servers, redirectors, all handler indexes, users and
registered functions. It expects servers to have been stopped first, as done by
`load-configuration`. Old routes and function registrations do not survive a
configuration reload. Application function definitions and logging settings are
not configuration registries and remain in place. Calling ordinary `load`
instead simply executes the file without the stop/reset step.

Handlers, redirects, users and registered functions are process-wide. They do
not belong to the preceding `server` form. Register them before starting servers
to avoid accepting requests during setup; use request host/port filters to
separate listeners. Application callbacks run concurrently.

### Starting and stopping listeners

```lisp
(polarcl:server :protocol :http :hostname "127.0.0.1" :port "8080"
                :mt-method :pooled :max-handlers 4)
```

`server` is a macro. Its required `:protocol` must be the literal `:http` or
`:https`; other arguments become server initialization arguments. It starts in
the background and returns the background thread, **not the server object**.
For individual shutdown, retain an explicitly created server:

```lisp
(defparameter *web*
  (make-instance 'polarcl:http-server
                 :hostname "127.0.0.1" :port "8081" :max-handlers 4))
(polarcl:run-http-server *web*)
;; Or use :background nil to run the accept loop in the calling thread.
(polarcl:stop-server *web*)
```

`run-http-server (server &key (background t))` starts the listener;
`stop-server (server)` sets its running flag to NIL; `stop-all-servers ()` does
this for all recorded servers. Shutdown is asynchronous: neither stop function
joins workers. Startup errors are logged inside the run loop, so a returned
thread alone does not establish successful binding.

### Server options

| Keyword | Default | Behavior |
| --- | --- | --- |
| `:hostname` | `"localhost"` | Local address/name to bind, not a virtual-host routing rule. |
| `:port` | `"8080"` | Service/port string; use strings consistently. |
| `:mt-method` | `:pooled` | Fixed worker pool; `:ondemand` creates work for accepted connections. |
| `:max-handlers` | `20` | Pool size or on-demand threshold. The on-demand counter is shared across servers. |
| `:keepalive` | `10000` | Milliseconds allowed while waiting for the next request line, passed to cl-mbedtls. |
| `:max-keepalive-total-time` | `10000` | Millisecond threshold for reusing a connection; currently measured per request cycle, not total connection age. |
| `:nodelay` | `t` | Passed to cl-mbedtls to disable Nagle's algorithm. |
| `:debug-level` | `0` | Passed to cl-mbedtls. |
| `:log-level` | `1` | Stored, but not applied by the server implementation. |
| `:database` | `"polarcl.sdb"` | Stored; not used for the built-in user registry. |
| `:default-charset` | `"UTF-8"` | Stored; request decoding instead uses the internal default `"utf-8"`. |

Keep-alive requires an explicit request `Connection: keep-alive` and an eligible
response. The emitted `Keep-Alive: timeout=5, max=100` header is hard-coded,
not derived from these settings. Headers/body reads use cl-mbedtls's separate
stream timeout.

Configure logging through log2, for example:

```lisp
(setf (log2:log-level "polarcl") log2:+info+)
```

### HTTPS

```lisp
(polarcl:server :protocol :https :hostname "127.0.0.1" :port "4443"
                :cert-file "/absolute/path/server-cert.pem"
                :key-file "/absolute/path/server-key.pem"
                :key-file-pw ""
                :max-handlers 4)
```

`:cert-file` and `:key-file` are passed as the server certificate and private
key to cl-mbedtls; `:key-file-pw` defaults to the empty string. Always supply your
own paths: the defaults point into the author's `/home/michael/certs/mbedTLS/`
directory. The HTTPS class itself is internal (`polarcl::https-server`); the
exported `server` macro is the public way to select it.

## Routes and handlers

```lisp
(polarcl:handle
 :request (:method :get :path "/health")
 :handler (:dynamic #'my-health-handler :authentication nil))
```

`handle` accepts two literal option lists. Request keys are `:host`, `:method`,
`:port`, and one of `:path`, `:prefix`, `:regex`. Method defaults to `:get`; a
literal list such as `(:get :post)` permits multiple methods. Host and port
constraints must evaluate to lists, e.g. `:host '("localhost:8081")` and
`:port '("8081")`; NIL is unrestricted. Host matching uses the full HTTP Host
header, including its port if present, and is case-insensitive.

| Match | Example | Meaning |
| --- | --- | --- |
| `:path` | `"/health"` | Exact, case-sensitive path. |
| `:prefix` | `"/assets"` | Exact prefix or prefix followed by `/`; matches `/assets/a.css`, not `/assets-old`. Omit trailing `/`. |
| `:regex` | `"^/reports/[0-9]+$"` | CL-PPCRE pattern; anchor explicitly when needed. |

Supply a path selector. `handle` without one constructs an exact NIL path,
not a catch-all. An empty prefix `""` matches all absolute paths.

Dispatch first checks redirects, then exact handlers, then the longest matching
prefix, then regex handlers. Most recently registered exact handlers win;
equal-length prefixes also favor the newest registration. Regex scoring uses
the request path length, so multiple matching regex handlers favor the newest.
Only one handler executes. No handler yields 404; handler errors normally yield
500 with the condition text in the body.

Handler options select one of:

| Option | Value / callback |
| --- | --- |
| `:static` | Filesystem path string; `file-handler`. |
| `:directory` | Filesystem root string; lists directories or serves files. Class is internal. |
| `:dynamic` | Function `(server handler request response)`; `dynhtml-handler`. Mutate response; return value is ignored. |
| `:query-function` | Non-NIL selects `qfunc-handler`; calls a registered function chosen by URL. |
| `:request-function` | Non-NIL selects `rfunc-handler`; calls a registered function chosen by URL. |
| `:websocket` | Function `(server handler request response)` after an upgrade handshake; experimental, see limitations. |

All accept `:realm`, `:authentication` and `:authorizer`. The `handle` macro
defaults to Basic authentication and NIL realm; explicitly use
`:authentication nil` for public dynamic/static content, or set a realm and user.
Select one handler kind per form.

### Static files

```lisp
;; Exact route maps to this complete filename.
(polarcl:handle :request (:path "/index.html")
                :handler (:static "/srv/site/index.html" :authentication nil))
;; /assets/app.css maps to /srv/site/assets/app.css.
(polarcl:handle :request (:prefix "/assets")
                :handler (:static "/srv/site/assets" :authentication nil))
;; Directory browsing is explicitly enabled here.
(polarcl:handle :request (:prefix "/downloads")
                :handler (:directory "/srv/downloads" :authentication nil))
```

Mapping replaces the matched path prefix by the root string using concatenation;
it does not resolve under `*content-root*`. An exact match therefore needs a
complete filename. Regex filters cannot map static/directory paths. Static
responses infer MIME type and support Last-Modified/If-Modified-Since. A missing
file currently raises a handler error (500), rather than returning 404.
`*content-root*` is exported and initialized to the source directory, but is not
used by these handlers. Static handlers do not implement uploads.

### Redirects

```lisp
(polarcl:redirect :from (:path "/") :to (:path "/index.html"))
(polarcl:redirect
 :from (:scheme "HTTP" :port ("8080") :prefix "")
 :to (:scheme "https" :port "4443"))
```

`redirect (&key from to)` defaults to GET. `:from` accepts `:method`, `:scheme`,
`:host`, `:port`, `:regex`, `:path`, `:prefix` (default empty string). Scheme and
method are normalized to lists by the macro; **port is quoted as supplied**,
so use `:port ("8080")`, not `:port "8080"` or a quoted list. Host is an
evaluated list as with `handle`. `:to` accepts `:scheme`, `:host`, `:port`,
`:path`, `:query`. Use a string for the target port.

Redirects return 302 before authentication. Unspecified scheme/host/path are
inherited; port comes from the target, then the Host header, then the selected
scheme's default. Specify the port when changing scheme. An absolute target
path replaces the path; a relative target appends to it with `/`. No regex
capture substitution is performed. `:query` is stored but unused and the source
query is not preserved. Redirect ordering is exact, prefix, regex; there is no
longest-prefix selection within redirects.

## Dynamic and registered functions

A dynamic callback sets the existing response:

```lisp
(defun my-health-handler (server handler request response)
  (declare (ignore server handler request))
  (setf (polarcl:http-header response :|Content-Type|) "application/json"
        (polarcl:http-body response) "{\"status\":\"ok\"}"))
```

`register-function (path &key symbol (authorizer #'function-authorizer))`
registers a **string key**. The last URL path segment is looked up in a global,
case-sensitive table: `/api/greet` looks up `"greet"`; earlier segments only
participate in route matching. Supplying `:symbol 'my-function` is the clearest
approach. Without it the key must be `"PACKAGE.SYMBOL"`; package names are
uppercased, but symbol names are looked up exactly as supplied.

Request functions receive `(handler request response)` and mutate the response;
the return value is ignored. Query functions receive those three arguments
followed by keyword/value pairs and their return value becomes `http-body`.
Values remain strings or NIL, not parsed numbers. Parameter names are interned
without changing case: `?name=Ada` supplies `:|name|`, not `:NAME`.

```lisp
(defun greet (handler request response &key (|name| "world"))
  (declare (ignore handler request))
  (setf (polarcl:http-header response :|Content-Type|) "text/plain")
  (format nil "Hello, ~a!" |name|))

(polarcl:register-function "greet" :symbol 'greet
 :authorizer (lambda (handler request registered-function)
               (declare (ignore handler request registered-function)) t))
(polarcl:handle :request (:path "/api/greet")
                :handler (:query-function t :authentication nil))
```

Registered-function authorization runs **even if handler authentication is NIL**.
Its default checks the built-in user registry. A public function thus needs an
explicit permissive three-argument authorizer as above. Unknown function names
raise an error. Unknown keywords can also error unless the callback accepts them.

## Authentication

```lisp
(polarcl:user :username "reader" :realm "reports" :password "example-only")
(polarcl:handle :request (:path "/private")
                :handler (:dynamic #'my-health-handler :realm "reports"))
```

With an HTTPS listener, try
`curl --cacert /path/to/ca.pem -u reader:example-only https://localhost:4443/private`.
Basic credentials are only base64 encoded on the wire; use HTTPS for real users.
The built-in registry stores passwords in plaintext despite its slot name
`hashed-password`. `user` is a keyword macro over
`register-user (name realm password)`; re-registering the same name/realm replaces
its entry. This is an in-memory registry, not the server's database file.

| Public function | Contract |
| --- | --- |
| `http-credentials (request)` | Returns username and password as two values from Basic credentials, or NIL when absent. |
| `authenticate (handler request)` | Calls the handler authorizer and caches success/failure on the request; returns a boolean. |
| `http-authenticated-user (handler request)` | Authenticates, then returns the values of `http-credentials`; primary value is the username. |
| `default-authorizer (handler request)` | Checks credentials against the handler realm's registered users. |
| `function-authorizer (handler request registered-function)` | Default function authorizer; delegates to `default-authorizer`. |
| `declining-authorizer (handler request registered-function)` | Always NIL; three-argument function-authorizer signature. |

A custom handler `:authorizer` takes two arguments and returns a truth value.
A registered-function authorizer takes three. Do not pass `declining-authorizer`
as a handler authorizer without an adapter. An authentication failure at the
handler or registered-function level returns 401 with a Basic challenge when the
authorizer returns NIL. The application callback is not called after denial.
Errors raised by user authorizers or application callbacks follow normal error
handling (500); they are not converted to authentication failures. An application
callback that explicitly sets the HTTP status keeps that status on normal return.

## Requests, responses and cookies

| Public API | Contract |
| --- | --- |
| `http-request`, `http-get` | Request classes; `http-get` subclasses `http-request`. These are not HTTP client functions. |
| `make-http-get (&rest args &key connection host port headers body path parameters fragment http-version)` | Constructs a request; normally the server does this. Although accepted in the function signature, `:host` is not a class initarg: use a Host header. |
| `path (request)` | Setf-able list of path segments, e.g. `("api" "greet")`, not a URL string. |
| `parameters (request)` | Setf-able list of two-element lists, e.g. `(("name" "Ada"))`. |
| `parse-url-query (string)` | Splits on `&` and first `=`; no percent decoding or `+` conversion. A key without `=` has NIL value. |
| `http-version (request)` | Setf-able string, normally `"HTTP/1.1"`. |
| `headers (message)` | Setf-able list of `http-header` objects. |
| `http-header` class | Construct with `:name` and `:value`; access through `field-name` and `field-value`. |
| `http-header (message name)` | Reads a header case-insensitively. Setf updates/adds a header by exact key identity; use consistent keyword spelling to avoid duplicates. |
| `http-body (message)` | Reads body; setf on responses handles compression and Content-Length. |
| `status-code`, `status-text` | Setf-able response accessors; set both, e.g. `"400"` and `"Bad Request"`. |
| `get-mime-for-extension (ext)` | MIME string for an extension without a dot; fallback is `"application/octet-stream"`. |
| `format-date-imf (stream time)` | Formats an HTTP date; integer universal time or local-time timestamp. NIL stream returns a string. |

Incoming header names are lowercase keywords (`:|content-type|`); use conventional
case-sensitive response keywords (`:|Content-Type|`). Initial successful responses
have status 200, `Content-Type: text/html`, `Access-Control-Allow-Origin: *`,
`Connection: keep-alive`, and `Server: PolarCL`. Set the body explicitly.

POST bodies are strings for text, JSON, XML and URL-encoded media types, and octet
vectors for application/octet-stream and image/png. JSON and form data are not
parsed into objects/parameters. Only POST reads a request body; although PUT is
recognized, it does not currently load its body. `Content-Length: 0` is explicitly
handled. HEAD is recognized but the response writer does not suppress its body.

Response strings/vectors may be compressed based on Accept-Encoding. Set
`Content-Encoding` to `"identity"` **before** setting the body to opt out.
Uncompressed strings currently calculate Content-Length in characters, which is
incorrect for non-ASCII UTF-8. Use UTF-8 octets for such responses:

```lisp
(setf (polarcl:http-header response :|Content-Type|) "text/plain; charset=utf-8"
      (polarcl:http-header response :|Content-Encoding|) "identity"
      (polarcl:http-body response) (babel:string-to-octets "Grüße" :encoding :utf-8))
```

`get-cookie (request name)` returns a cookie object or NIL. Read or set its
exported `cookie-name`, `cookie-value`, `cookie-options` accessors.
`set-cookie (response name value &key expires max-age options)` appends a
Set-Cookie header. Default max-age is 30 days (seconds); options default to
`("Secure" "HttpOnly")`. `expires` accepts a date value supported by
`format-date-imf`. Setting cookies on requests is unimplemented.

```lisp
(let ((cookie (polarcl:get-cookie request "theme")))
  (when cookie (polarcl:cookie-value cookie)))
(polarcl:set-cookie response "theme" "dark" :max-age 3600
                    :options '("Secure" "HttpOnly" "Path=/" "SameSite=Lax"))
```

## Lower-level registration API

For programmatically assembled configuration, use the exported constructors:

```lisp
(polarcl:register-handler
 :filter (polarcl:create-filter 'polarcl:exact-filter
           :protocol '("HTTP" "HTTPS") :port '("8081")
           :method '(:get) :path "/health")
 :handler (polarcl:create-handler 'polarcl:dynhtml-handler
            :contentfn #'my-health-handler :authentication nil))
```

`create-filter (type &rest args &key protocol port host method path)` forwards
supplied arguments to `make-instance`. Use `exact-filter`, `prefix-filter` or
`regex-filter`; protocol, port, host and method are lists or NIL. Omitted options
use **class defaults**, not the apparent keyword defaults on the constructor:
protocols HTTP/HTTPS, ports 80/443/8080/4443, all five supported methods, NIL host,
and `/index.html`. By contrast, `handle` passes NIL port (any port) and GET.

`create-handler (type &rest args &key method redirector realm authentication
 authorizer rootdir wsfunction contentfn)` likewise forwards arguments. Use the
exported `file-handler`, `dynhtml-handler`, `qfunc-handler`, `rfunc-handler` and
only initargs supported by that class. `:method` and `:redirector` appear in the
signature but are not handler class initargs. Direct handler creation defaults
to NIL authentication and realm `"root"`, unlike the `handle` macro.

`register-handler (&key filter handler)` installs a route.
`create-redirector (&key scheme host port path query)` constructs a redirector;
`register-redirector (&key filter redirector)` installs it. Registration functions
mutate global state; their return values are not stable route handles. There is
no exported per-route removal API.

## Implementation boundaries and older examples

The snippets in this manual and `examples/api-config.cl` follow the current
code. Older files under `examples/` contain obsolete forms: missing `:protocol`,
positional `user`, `:max-threads`, scalar filter ports, symbol-only
`register-function`, and incorrect callback arities. Do not copy them unchanged.

WebSocket configuration performs a handshake and calls the four-argument callback,
but there is no public frame read/write API; ordinary response processing also
continues afterward. Treat it as experimental, not a complete WebSocket service.
Digest authentication, static uploads and automatic JSON serialization are not
implemented. Request parsing and response compression have limited negotiation;
this manual describes actual behavior rather than claiming full HTTP compliance.

Implementation map: [configuration.cl](configuration.cl) contains the configuration
macros; [server.cl](server.cl) the listener and parser;
[handlers.cl](handlers.cl) routing, files and authentication;
[registered-functions.cl](registered-functions.cl) function dispatch;
[messages.cl](messages.cl) request/response accessors;
[cookies.cl](cookies.cl) cookies.
