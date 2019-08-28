type t = {
  iat : int;     (* Issued at time *)
  exp : int;     (* JWT expiration time (10 minute maximum) *)
  iss : string;  (* GitHub App's identifier *)
} [@@deriving to_yojson]

let b64encode = Base64.(encode_exn ~pad:false ~alphabet:uri_safe_alphabet)

let header = b64encode {|{"typ":"JWT","alg":"RS256"}|} ^ "."

(* https://tools.ietf.org/html/rfc3447#page-43 *)
let sha256_asn = Cstruct.of_string
  "\x30\x31\x30\x0d\x06\x09\x60\x86\x48\x01\x65\x03\x04\x02\x01\x05\x00\x04\x20"

let sign ~key msg =
  let digest = Nocrypto.Hash.digest `SHA256 msg in
  Nocrypto.Rsa.PKCS1.sig_encode ~key (Cstruct.append sha256_asn digest)

let encode ~key ~iat ~app_id =
  let exp = iat + 10 * 60 in
  let t = { iat; exp; iss = app_id } in
  let payload = to_yojson t |> Yojson.Safe.to_string |> b64encode in
  let data = header ^ payload in
  let signature = sign ~key (Cstruct.of_string data) in
  Printf.sprintf "%s.%s" data (b64encode (Cstruct.to_string signature))
