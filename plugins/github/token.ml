type t = {
  iat : int;     (* Issued at time *)
  exp : int;     (* JWT expiration time (10 minute maximum) *)
  iss : string;  (* GitHub App's identifier *)
} [@@deriving to_yojson]

let b64encode = Base64.(encode_exn ~pad:false ~alphabet:uri_safe_alphabet)

let header = b64encode {|{"typ":"JWT","alg":"RS256"}|} ^ "."

let encode ~key ~iat ~app_id =
  let exp = iat + 10 * 60 in
  let t = { iat; exp; iss = app_id } in
  let payload = to_yojson t |> Yojson.Safe.to_string |> b64encode in
  let data = header ^ payload in
  let signature =
    let msg = Cstruct.of_string data in
    Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key (`Message msg)
  in
  Printf.sprintf "%s.%s" data (b64encode (Cstruct.to_string signature))
