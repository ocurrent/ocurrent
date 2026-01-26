module Engine = Engine
module Job = Job
module Client = Client

(* We need to capture references to the impl module before defining our functors *)
module Impl_internal = Impl

module Impl (Current : S.CURRENT) = Impl_internal.Make(Current)
module Impl_with_db (Current : S.CURRENT) (Db : S.DB) = Impl_internal.Make_with_db(Current)(Db)
module S = S
