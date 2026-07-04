(** A lightweight proof-of-work browser challenge.

    This implements the idea behind tools like Anubis natively: a request for an
    HTML page from a client without a valid token is served an interstitial page
    whose embedded JavaScript must find a nonce [n] such that
    [SHA-256(challenge ^ ":" ^ n)] has at least [difficulty] leading zero bits.
    On success the client is issued a short signed cookie and let through.

    Real browsers solve this transparently in well under a second; HTTP-only
    crawlers (which do not execute JavaScript) never obtain a token. It is
    entirely stateless on the server: both the challenge and the token are
    HMAC-signed with a per-process (or supplied) secret. *)

type t

val v :
  ?secret:string ->
  ?difficulty:int ->
  ?token_ttl:float ->
  ?challenge_ttl:float ->
  ?cookie_name:string ->
  ?protect:(string -> bool) ->
  unit -> t
(** [v ()] is a challenge configuration.
    @param secret HMAC key for signing challenges and tokens. Defaults to 32
      random bytes generated at startup (so tokens are invalidated on restart).
    @param difficulty Number of leading zero bits required of the PoW hash
      (default 16). Note that any value defeats non-JavaScript crawlers; the
      difficulty only throttles JavaScript-capable clients.
    @param token_ttl Lifetime of an issued token cookie, in seconds (default 1 week).
    @param challenge_ttl How long a freshly issued challenge may be solved for,
      in seconds (default 10 minutes).
    @param cookie_name Name of the token cookie (default ["__ocurrent_pow"]).
    @param protect Predicate on the request path. A [`GET] whose path satisfies
      it is challenged regardless of the [Accept] header (default [fun _ ->
      false], i.e. only [Accept: text/html] GETs are challenged). Use this to
      protect expensive pages against crawlers that send [Accept: */*] to skip
      the header check; e.g. [~protect:(String.starts_with ~prefix:"/job/")].
      Keep it off asset/metrics paths so the interstitial can still load. *)

val handle :
  t ->
  secure:bool ->
  Cohttp.Request.t ->
  path:string ->
  meth:Cohttp.Code.meth ->
  [ `Pass | `Response of (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t ]
(** [handle t ~secure request ~path ~meth] is the gate.
    It returns [`Response r] to serve the interstitial (for an HTML page request
    lacking a valid token), to handle the verification endpoint, or to serve the
    interstitial's own JS/CSS assets (so a bare server without static routes can
    present a solvable challenge); otherwise [`Pass], meaning the request should
    be routed as normal. Only
    [`GET] requests that either accept [text/html] or whose path matches the
    [protect] predicate are ever challenged, so static assets, [/metrics] and
    webhook POSTs pass through untouched.
    @param secure Whether to set the [Secure] attribute on the cookie. *)
