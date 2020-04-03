val encode : key:Mirage_crypto_pk.Rsa.priv -> iat:int -> app_id:string -> string
(** [encode ~key ~iat ~app_id] is a GitHub token for app [app_id], issued
    at time [iat] (i.e. now) and signed with [key]. The resulting token
    is valid for one hour. *)
