type 'a t = 'a Modifiable.t
type 'a cc = 'a t -> Modifiable.changeable
type 'a var = 'a t

let var x =
  Modifiable.create (fun d -> Modifiable.write ~eq:(==) d x)

let of_var x = x

(* For now, a constant is just a variable that the type system won't let you change.
   But we could be more efficient, since we don't need to record readers of a const. *)
let const = var

let write ?(eq=(==)) x d = Modifiable.write ~eq d x

let read x f d = Modifiable.read x (fun y -> f y d)

let of_cc = Modifiable.create

let map f x = of_cc (read x (fun x -> write (f x)))

let change ?(eq=(==)) = Modifiable.change ~eq
let propagate = Modifiable.propagate
let observe = Modifiable.deref
let on_release = Modifiable.on_release
