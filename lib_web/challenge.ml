module Server = Cohttp_lwt_unix.Server

type t = {
  secret : string;
  difficulty : int;
  token_ttl : float;
  challenge_ttl : float;
  cookie_name : string;
  protect : string -> bool;
}

(* The endpoint the interstitial's JavaScript calls once it has solved the
   proof-of-work. Intercepted by [handle] before normal routing. *)
let verify_path = "/.ocurrent-challenge/verify"

let v ?secret ?(difficulty = 16) ?(token_ttl = 604800.) ?(challenge_ttl = 600.)
    ?(cookie_name = "__ocurrent_pow") ?(protect = fun _ -> false) () =
  let secret =
    match secret with
    | Some s -> s
    | None -> Cstruct.to_string (Mirage_crypto_rng.generate 32)
  in
  { secret; difficulty; token_ttl; challenge_ttl; cookie_name; protect }

let hmac_hex t msg =
  Digestif.SHA256.hmac_string ~key:t.secret msg |> Digestif.SHA256.to_hex

(* Constant-time string equality (assumes [a] is the trusted-length value). *)
let ct_eq a b =
  if String.length a <> String.length b then false
  else begin
    let acc = ref 0 in
    String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
    !acc = 0
  end

let now () = Unix.time ()

(* "<exp>.<sig>" where sig = HMAC("tok:" ^ exp). *)
let make_token t =
  let exp = Printf.sprintf "%.0f" (now () +. t.token_ttl) in
  exp ^ "." ^ hmac_hex t ("tok:" ^ exp)

let valid_token t s =
  match String.index_opt s '.' with
  | None -> false
  | Some i ->
    let exp = String.sub s 0 i in
    let sg = String.sub s (i + 1) (String.length s - i - 1) in
    ct_eq (hmac_hex t ("tok:" ^ exp)) sg
    && (match float_of_string_opt exp with Some e -> e > now () | None -> false)

(* "<ts>.<sig>" where sig = HMAC("chal:" ^ ts). Binds the challenge to our
   secret and a time, so a client cannot mint its own. *)
let make_challenge t =
  let ts = Printf.sprintf "%.0f" (now ()) in
  ts ^ "." ^ hmac_hex t ("chal:" ^ ts)

let valid_challenge t c =
  match String.index_opt c '.' with
  | None -> false
  | Some i ->
    let ts = String.sub c 0 i in
    let sg = String.sub c (i + 1) (String.length c - i - 1) in
    ct_eq (hmac_hex t ("chal:" ^ ts)) sg
    && (match float_of_string_opt ts with
        | Some t0 -> now () -. t0 <= t.challenge_ttl
        | None -> false)

(* Number of leading zero bits of a raw (binary) string. *)
let leading_zero_bits s =
  let clz_byte b =
    let rec go n = if n = 8 || (b lsr (7 - n)) land 1 = 1 then n else go (n + 1) in
    go 0
  in
  let n = String.length s in
  let rec go i acc =
    if i >= n then acc
    else
      let c = Char.code s.[i] in
      if c = 0 then go (i + 1) (acc + 8) else acc + clz_byte c
  in
  go 0 0

let pow_ok t ~challenge ~nonce =
  let h = Digestif.SHA256.(digest_string (challenge ^ ":" ^ nonce) |> to_raw_string) in
  leading_zero_bits h >= t.difficulty

(* Only allow same-site redirect targets (a local absolute path). *)
let safe_redirect = function
  | Some r
    when String.length r >= 1 && r.[0] = '/'
         && not (String.length r >= 2 && r.[1] = '/')
         && not (String.contains r '\n') && not (String.contains r '\r') ->
    r
  | _ -> "/"

let cookie t ~secure value =
  let attrs =
    [ Printf.sprintf "%s=%s" t.cookie_name value;
      "Path=/";
      Printf.sprintf "Max-Age=%d" (int_of_float t.token_ttl);
      "HttpOnly";
      "SameSite=Lax" ]
  in
  let attrs = if secure then attrs @ [ "Secure" ] else attrs in
  String.concat "; " attrs

let get_cookie t request =
  match Cohttp.Header.get (Cohttp.Request.headers request) "cookie" with
  | None -> None
  | Some s ->
    String.split_on_char ';' s
    |> List.find_map (fun kv ->
        match String.index_opt kv '=' with
        | None -> None
        | Some i ->
          let k = String.trim (String.sub kv 0 i) in
          let v = String.sub kv (i + 1) (String.length kv - i - 1) in
          if k = t.cookie_name then Some v else None)

let interstitial_body ~challenge ~difficulty ~redirect =
  let open Tyxml.Html in
  let doc =
    html
      (head (title (txt "One moment…"))
         [ meta ~a:[a_charset "utf-8"] ();
           meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1.0"] ();
           meta ~a:[a_name "robots"; a_content "noindex,nofollow"] ();
           link ~rel:[`Stylesheet] ~href:"/css/challenge.css" () ])
      (body
         [ div
             ~a:[ a_id "challenge";
                  a_class ["challenge"];
                  a_user_data "challenge" challenge;
                  a_user_data "difficulty" (string_of_int difficulty);
                  a_user_data "redirect" redirect;
                  a_user_data "verify" verify_path ]
             [ h1 [txt "Just a moment…"];
               p [txt "Your browser is solving a small proof-of-work puzzle to keep automated scrapers out."];
               p ~a:[a_id "status"] [txt "Working…"];
               noscript [p [txt "JavaScript is required to solve the proof-of-work puzzle."]] ];
           script ~a:[a_src "/js/challenge.js"] (txt "") ])
  in
  Fmt.str "%a" (pp ()) doc

let respond_interstitial t request =
  let challenge = make_challenge t in
  let redirect =
    Uri.path_and_query (Cohttp.Request.uri request) |> Option.some |> safe_redirect
  in
  let body = interstitial_body ~challenge ~difficulty:t.difficulty ~redirect in
  let headers =
    Cohttp.Header.of_list
      [ ("Content-Type", "text/html; charset=utf-8");
        ("Cache-Control", "no-store") ]
    |> Utils.add_security_headers
  in
  (* 503 so crawlers/caches treat it as "not the content"; browsers still run the JS. *)
  Server.respond_string ~status:`Service_unavailable ~headers ~body ()

(* The interstitial's own assets. Served by [handle] so that a bare server
   (one not using [Site]'s crunch routes) can still present a solvable
   challenge: the browser must be able to fetch these to run the solver. *)
let assets =
  [ "/js/challenge.js", "text/javascript; charset=utf-8";
    "/css/challenge.css", "text/css; charset=utf-8" ]

let respond_asset ~content_type body =
  let headers =
    Cohttp.Header.of_list
      [ ("Content-Type", content_type);
        ("Cache-Control", "public, max-age=86400") ]
    |> Utils.add_security_headers
  in
  Server.respond_string ~status:`OK ~headers ~body ()

let respond_verify t ~secure request =
  let uri = Cohttp.Request.uri request in
  let challenge = Uri.get_query_param uri "c" in
  let nonce = Uri.get_query_param uri "n" in
  let redirect = safe_redirect (Uri.get_query_param uri "r") in
  match challenge, nonce with
  | Some challenge, Some nonce
    when valid_challenge t challenge && pow_ok t ~challenge ~nonce ->
    let headers =
      Cohttp.Header.of_list
        [ ("Location", redirect);
          ("Set-Cookie", cookie t ~secure (make_token t));
          ("Cache-Control", "no-store") ]
    in
    Server.respond ~status:`Found ~headers ~body:Cohttp_lwt.Body.empty ()
  | _ ->
    (* Bad / stale solution: re-issue a fresh challenge. *)
    respond_interstitial t request

let handle t ~secure request ~path ~meth =
  if path = verify_path then `Response (respond_verify t ~secure request)
  else if meth = `GET && List.mem_assoc path assets then
    (match Static.read path with
     | Some body -> `Response (respond_asset ~content_type:(List.assoc path assets) body)
     | None -> `Pass)
  else
    (* Challenge a GET when the client asks for HTML (a browser navigation) OR
       when the path is one the site marked as protected. The latter closes the
       gap where a crawler sends [Accept: */*] (or no Accept) to an expensive
       page and is served the full content without ever seeing the challenge:
       [protect] gates by path, which the client cannot spoof away. Static
       assets, [/metrics] and webhook POSTs still pass (not GET-html, not
       protected), so the interstitial's own css/js load and the browser can
       solve it. *)
    let should_challenge =
      meth = `GET
      && (t.protect path
          || (match Cohttp.Header.get (Cohttp.Request.headers request) "accept" with
              | Some a -> Astring.String.is_infix ~affix:"text/html" a
              | None -> false))
    in
    if not should_challenge then `Pass
    else
      match get_cookie t request with
      | Some tok when valid_token t tok -> `Pass
      | _ -> `Response (respond_interstitial t request)
