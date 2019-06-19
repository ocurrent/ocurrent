(** Each operation has a level, giving an estimate of the cost or risk of the operation.
    When testing a pipeline, it can be useful to set a maximum level, or to require
    confirmation for more risky operations. *)

type t =
  | Harmless            (** e.g. reading from the local filesystem *)
  | Mostly_harmless     (** e.g. performing a remote query *)
  | Average             (** e.g. building a binary from source *)
  | Above_average       (** e.g. sending a notification *)
  | Dangerous           (** e.g. publishing a new release *)
[@@deriving ord]

val values : t list
(** All the levels, from least to most dangerous. *)

val pp : t Fmt.t
val to_string : t -> string
val of_string : string -> (t, [> `Msg of string]) result
