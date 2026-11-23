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

All require an explicit `:authentication` choice: NIL for public access, or a
scheme policy described below. Select one handler kind per form.

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

`register-function (path &key symbol)`
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

(polarcl:register-function "greet" :symbol 'greet)
(polarcl:handle :request (:path "/api/greet")
                :handler (:query-function t :authentication nil))
```

Registered functions inherit their matched route's authentication policy. There
is no function-level authentication or authorization callback. Unknown function
names raise an error. Unknown keywords can also error unless the callback accepts
them.

## Authentication

PolarCL performs one authentication decision after route selection and before
calling the content handler. Registered functions do not authenticate again.
Authorization (roles, permissions, scopes), account management, OAuth/OIDC login,
SSO, JWT verification and token issuance are application concerns. Providers can
integrate external identity systems without changing HTTP dispatch.

Every `handle` form must explicitly choose `:authentication nil` or one scheme:

```lisp
(polarcl:handle :request (:prefix "/public")
 :handler (:static "/srv/public" :authentication nil))

(polarcl:user :username "reader" :realm "reports" :password "example-only")
(polarcl:handle :request (:path "/private")
 :handler (:dynamic #'my-health-handler
           :authentication (:basic :realm "reports" :provider :local-users)))
```

The scheme specification is a literal list; its option values are evaluated.
A missing authentication option, unknown scheme/provider, missing realm, or old
`:authorizer`/handler-level `:realm` option is a configuration error. One scheme
is supported per route. Providers must be registered before routes using them.

Public routes ignore authentication credentials and expose no principal. A
protected route verifies each request separately, including on persistent
connections. It never treats a previous request's identity as proof for the next.
All handler kinds use this mechanism. A successful callback may set its own HTTP
status; authentication does not overwrite it.

### Policies and transport

| Policy option | Meaning |
| --- | --- |
| `:realm` | Required nonempty printable ASCII protection-space label, not a role. Quotes and backslashes are escaped in challenges. |
| `:provider` | Named credential provider; defaults to `:local-users`, `:local-tokens`, or `:local-digest` for the selected scheme. |
| `:require-tls` | Defaults to T. Basic/Digest permit an explicit NIL for local development. Bearer always requires TLS. |
| `:nonce-lifetime` | Digest lifetime in seconds; default 300, allowed 1–3600. |

TLS is determined from the actual connection; untrusted forwarded headers do not
assert a secure connection. TLS termination in a reverse proxy is not integrated
in this version. An insecure request to a TLS-required route gets 403 without
calling the provider. With an HTTPS listener, try:

```sh
curl --cacert /path/to/ca.pem -u reader:example-only https://localhost:4443/private
```

### Built-in credential stores

`user` expands to `register-user (name realm password &key provider principal)`.
Basic passwords are normalized to NFC and stored only as salted
PBKDF2-HMAC-SHA256 verifiers (600,000 iterations, random 16-byte salt). The default
principal has the username as ID and `:local-users` as source. Passwords containing
colons are supported; usernames containing colons or control characters are not.
The Basic challenge advertises `charset="UTF-8"`; this encoding is independent
of the request body's Content-Type charset.

Bearer tokens are opaque, high-entropy application-issued secrets. PolarCL stores
their SHA-256 hashes, principal and optional expiry, not the raw token:

```lisp
(polarcl:register-token application-issued-token
 (polarcl:make-principal :id "routing-client" :source :my-application)
 :realm "api" :expires (+ (get-universal-time) 3600))
(polarcl:handle :request (:prefix "/api")
 :handler (:query-function t
           :authentication (:bearer :realm "api" :provider :local-tokens)))
;; Revoke the exact registration:
(polarcl:revoke-token application-issued-token :realm "api")
```

`register-token (token principal &key provider realm expires)` uses universal time;
NIL expiry means no expiration. Only the Authorization header transport is
accepted. Query/form tokens are not authentication inputs. Possessing a token
establishes its verified principal context, not necessarily a human identity.
No scopes are enforced by PolarCL.

Digest uses a separate credential store because password verifiers for Basic
cannot be used to calculate Digest proofs:

```lisp
(polarcl:register-digest-user "reader" "reports" :password "example-only")
(polarcl:handle :request (:path "/digest")
 :handler (:dynamic #'my-health-handler
           :authentication (:digest :realm "reports" :provider :local-digest)))
```

`register-digest-user (name realm &key password ha1 provider principal)` requires
exactly one of password or HA1. HA1 is the 64-digit SHA-256 hexadecimal digest of
UTF-8 `username:realm:password`; username/password are normalized to NFC when
provisioning from a password. HA1 is password-equivalent sensitive material.

The supported Digest profile is SHA-256, `qop=auth`, UTF-8 (including
`username*` extended parameters). It validates the exact
method and request target (including query), nonce lifetime and increasing nonce
counts per username/cnonce. Expired, otherwise valid proofs receive `stale=true`.
Nonces are bound to the route policy. Replay checks and updates are atomic.
Nonce storage is bounded at 4,096 entries, each with at most 4,096 counter keys;
capacity exhaustion fails closed. Reset/restart invalidates nonces. This is
process-local state, not a distributed replay store. SHA-256-sess, MD5,
SHA-512/256, `auth-int`, userhash and proxy authentication are not implemented.

```sh
curl --cacert /path/to/ca.pem --digest -u reader:example-only \
  https://localhost:4443/digest
```

Stores are in memory. Re-registering a key replaces its entry. Configuration
reset clears local users, tokens, Digest credentials, providers and nonce state.
The crypto backend uses native mbedTLS 3's PBKDF2 API and stateless SHA-256 API;
randomness comes from `/dev/urandom`, and Unicode normalization uses SBCL.

### Principal and provider API

`request-principal (request)` returns a `principal` or NIL. Its public accessors
are `principal-id`, `principal-source`, and `principal-attributes`.
`request-authentication` returns an authentication context or NIL; inspect it
with `authentication-context-principal` and `authentication-context-scheme`.
Passwords and bearer tokens are not stored in the principal/context.

Register an application provider with
`register-authentication-provider (name scheme function)`. The function receives
`(policy request credentials)` and returns either
`(authentication-success principal)` or `(authentication-rejected)`.

```lisp
(polarcl:register-authentication-provider :external-tokens :bearer
 (lambda (policy request credentials)
   (declare (ignore policy request))
   ;; Application code validates issuer, audience, signature/expiry, revocation,
   ;; etc. or performs introspection. Merely decoding a JWT is not validation.
   (let ((identity (validate-access-token (polarcl:credentials-token credentials))))
     (if identity
         (polarcl:authentication-success
          (polarcl:make-principal :id identity :source :external-issuer))
         (polarcl:authentication-rejected)))))
```

The callback can inspect `authentication-policy-realm` and
`authentication-policy-scheme`. Basic credentials expose `credentials-username`
and `credentials-password`; Bearer exposes `credentials-token`; Digest supplies
the username. A Digest provider returns
`(authentication-success principal :digest-ha1 hash)` so PolarCL can verify the
proof; provider success alone does not authenticate Digest. A provider returning
NIL or another invalid result is an application error, not a credential rejection.
Providers must be safe for concurrent calls. They do not receive a response to
mutate; PolarCL owns challenge/status generation. No automatic retries occur.

### Failures, RFC scope and migration

| Outcome | Response |
| --- | --- |
| Missing credentials or an unsupported credential scheme | 401 with the route's challenge; no application callback. |
| Rejected credentials | 401; Bearer adds `error="invalid_token"`. |
| Malformed credentials or duplicate Authorization fields | 400; Bearer adds `error="invalid_request"`. |
| TLS required but absent | 403. |
| Provider or application exception | 500 with generic text, not exception details. |
| Application sets a response status | Preserved on normal callback return. |

All 401 responses contain WWW-Authenticate. Missing Bearer credentials do not
include an error parameter. Credential-bearing headers are redacted when printed;
request-header and request-body trace dumps are disabled. Applications must also
avoid logging credentials in their own providers.

The framework follows [RFC 9110 §11](https://www.rfc-editor.org/rfc/rfc9110.html#section-11),
which supersedes RFC 7235, with the profiles above for
[Basic (RFC 7617)](https://www.rfc-editor.org/rfc/rfc7617.html),
[Bearer (RFC 6750)](https://www.rfc-editor.org/rfc/rfc6750.html), and
[Digest (RFC 7616)](https://www.rfc-editor.org/rfc/rfc7616.html).
This is an origin-server implementation, not an OAuth authorization server or
HTTP proxy. Optional authentication, multiple schemes per route, sessions,
role/scope policies and built-in identity-provider integrations are outside this
version. Provider hooks leave room for separate integration libraries.

Migration is intentional: remove handler/function `:authorizer` options and
handler-level `:realm`; select a policy or explicit NIL on each HANDLE.
`create-handler` is internal. Legacy `authenticate`, `http-credentials`,
`http-authenticated-user`, `default-authorizer`, `function-authorizer` and
`declining-authorizer` are no longer public APIs. Read the verified principal
instead of reparsing credentials. Bitsailor's routes are public; the old HTTP
`/quit` route has been removed rather than exposing unauthenticated shutdown.

## Requests, responses and cookies

| Public API | Contract |
| --- | --- |
| `http-request`, `http-get` | Request classes; `http-get` subclasses `http-request`. These are not HTTP client functions. |
| `make-http-get (&rest args &key connection port headers body path parameters fragment http-version)` | Constructs a request; normally the server does this. Supply the host through a Host header; there is no `:host` constructor keyword. |
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

The unused server option `:default-charset` has been removed. Text request bodies
use the charset specified in Content-Type (including quoted values and
case-insensitive parameter names), defaulting to UTF-8 when it is absent.
Unsupported declared charsets raise a decoding error rather than silently using
UTF-8. Transfer chunks are joined before decoding, so characters may span chunks.

POST and PUT bodies are strings for text, JSON, XML and URL-encoded media types, and octet
vectors for application/octet-stream and image/png. JSON and form data are not
parsed into objects/parameters. PUT uses the same body reader as POST, including
charset decoding and supported compression/transfer encodings. `Content-Length: 0` is explicitly
handled. HEAD responses send the generated headers, including Content-Length,
but omit the body on the wire. The route must explicitly permit `:head` when
using `handle`, whose default method is GET.

PUT support covers parsing, routing and making the body available to callbacks.
Register a route with `:method :put` and read `(http-body request)` in its handler.
The application must implement resource creation/replacement and choose the
response status. There are no automatic filesystem writes; a static-file handler
still reads files even if its route permits PUT. For example, this diagnostic
endpoint echoes a text PUT body without storing it:

```lisp
(polarcl:handle
 :request (:method :put :path "/put-echo")
 :handler (:dynamic (lambda (server handler request response)
                      (declare (ignore server handler))
                      (setf (polarcl:http-header response :|Content-Type|)
                            "text/plain; charset=utf-8"
                            (polarcl:http-body response)
                            (polarcl:http-body request)))
           :authentication nil))
```

```sh
curl -i -X PUT -H 'Content-Type: text/plain; charset=utf-8' \
  --data-binary 'Grüße' http://127.0.0.1:8080/put-echo
```

Response strings/vectors may be compressed based on Accept-Encoding. Set
`Content-Encoding` to `"identity"` **before** setting the body to opt out.
Strings are sent as UTF-8, and Content-Length counts encoded bytes, not characters
(for example, `"Grüße"` is 7 bytes). Compressed responses count the compressed
bytes. HEAD reports the same length as GET while sending no body. These rules
follow [RFC 9110 §8.6](https://www.rfc-editor.org/rfc/rfc9110.html#section-8.6).
Strings remain strings through `http-body` when compression is not applied;
applications can also supply an octet vector directly.

```lisp
(setf (polarcl:http-header response :|Content-Type|) "text/plain; charset=utf-8"
      (polarcl:http-header response :|Content-Encoding|) "identity"
      (polarcl:http-body response) "Grüße")
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

## Lower-level registration internals

Applications should use HANDLE. The following illustrates internal construction
for maintainers; `create-handler` is no longer exported:

```lisp
(polarcl:register-handler
 :filter (polarcl:create-filter 'polarcl:exact-filter
           :protocol '("HTTP" "HTTPS") :port '("8081")
           :method '(:get) :path "/health")
 :handler (polarcl::create-handler 'polarcl:dynhtml-handler
            :contentfn #'my-health-handler :authentication nil))
```

`create-filter (type &rest args &key protocol port host method path)` forwards
supplied arguments to `make-instance`. Use `exact-filter`, `prefix-filter` or
`regex-filter`; protocol, port, host and method are lists or NIL. Omitted options
use **class defaults**, not the apparent keyword defaults on the constructor:
protocols HTTP/HTTPS, ports 80/443/8080/4443, all five supported methods, NIL host,
and `/index.html`. By contrast, `handle` passes NIL port (any port) and GET.

`create-handler (type &rest args &key authentication rootdir wsfunction contentfn)`
is internal and forwards arguments to `make-instance`. Its authentication value
is NIL or an internal validated policy object. Class-specific initargs apply only
to the corresponding handler type. Applications use HANDLE to validate policies.

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
Static uploads and automatic JSON serialization are not implemented. Request parsing and response compression have limited negotiation;
this manual describes actual behavior rather than claiming full HTTP compliance.

Implementation map: [configuration.cl](configuration.cl) contains the configuration
macros; [server.cl](server.cl) the listener and parser;
[handlers.cl](handlers.cl) routing and files;
[authentication.cl](authentication.cl) authentication policies, stores and protocols;
[registered-functions.cl](registered-functions.cl) function dispatch;
[messages.cl](messages.cl) request/response accessors;
[cookies.cl](cookies.cl) cookies.
