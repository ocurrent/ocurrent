# Internals 

To use OCurrent effectively, it can be useful to understand how it works internally.
This page describes how OCurrent is implemented.
There are three main features provided by a `Current.t` value:

- Automatic re-evaluation when inputs change.
- Handling of errors and pending states.
- Static analysis and diagram generation.

Let's look at these individually...

## Incremental evaluation

The automatic updating is built on the `current_incr` library (in `lib_incr` in the source).
This library is very small and has no dependencies, and can be useful even outside of OCurrent.

Let's try it out. You can get an interactive environment with:

```
opam depext -i current utop
utop
```

```ocaml
# #require "current"
# open Current_incr

# let x = Current_incr.var 3
...
# let y = Current_incr.map (fun x -> x * 2) (Current_incr.of_var x)
...
# Current_incr.map (Printf.printf "y is now %d\n") y
y is now 6
...
```

This is the incremental equivalent of:

```ocaml
# let x' = 3
...
# let y' = x' * 2
...
# Printf.printf "y' is now %d\n" y'
y' is now 6
...
```

However, we can change `x` and recompute everything that depends on it:

```ocaml
# Current_incr.change x 21
...
# Current_incr.propagate ()
y is now 42
...
```

If you run `propagate` again, it will not display `y is now 42` again, as the value hasn't changed.

For more information about this library, see the [Current_incr API docs](https://ocurrent.github.io/ocurrent/current_incr/Current_incr/index.html).

## Errors and pending results

The `lib_term` library depends on and wraps `current_incr`.
It adds static analysis (which we'll look at in the next section) and error handling.
Normally it is wrapped in turn by the main OCurrent library, but we can use it directly like this:

```ocaml
# #require "current.term"
# module Term = Current_term.Make(Unit)
...
```

The functor argument just provides a type for job metadata, which we don't care about for this example.
Here are some examples showing how errors are handled:

```ocaml
# let test x =
         x
   |> Term.map (fun x -> x * 2)
   |> Term.Executor.run
   |> Current_incr.observe
...
# test (Term.return 21)
- : int Current_term__.Output.t = Ok 42

# test (Term.fail "Crashed")
- : int Current_term__.Output.t = Error (`Msg "Crashed")

# test (Term.active `Running)
- : int Current_term__.Output.t = Error (`Active `Running)
```

This is a pretty standard result monad, except that we have an extra "error"
type to represent a calculation that couldn't produce a result yet, but will in
future.

You can use `Term.catch` to turn a failed value back into a usable result:

```ocaml
# Term.fail "Crashed"
       |> Term.catch
       |> Term.map (function
          | Ok _ -> "good"
          | Error _ -> "bad")
       |> Term.Executor.run
       |> Current_incr.observe
- : string Current_term__.Output.t = Ok "bad"
```

This pipeline catches the error at the start and successfully returns "bad".

`Term.state` is similar, but also gives access to active values.
For example, when testing a PR on GitHub your pipeline may want to handle the "active" state to set the GitHub commit status to "pending".

To make any real use of `Term`, you'll need to provide some "primitive" operations that do something interesting.
For example:

```ocaml
# module Term = Current_term.Make(Unit)
...
# open Term.Syntax

# let approved = Current_incr.var false
...
# let await_approval x =
  Term.component "approve" |>
  let> x = x in
  Current_incr.of_cc begin
    Current_incr.read (Current_incr.of_var approved) @@ function
    | true -> Current_incr.write (Ok x, None)
    | false -> Current_incr.write (Error (`Active (`Ready)), None)
  end
...
```

`await_approval x` is a term that is pending until `approved` is `true`, then returns `x`.
`Term.component` provides a label for the static analysis, `let> x = x` gets the current value of term `x` (and records the static dependency on `x`), and the body is an incremental value with the result.
You can use it like this:

```ocaml
# let result = Term.return ~label:"build result" "image1" |> await_approval
...
# Term.Executor.run result |> Current_incr.observe
- : string Current_term__.Output.t = Error (`Active `Ready)
# Current_incr.change approved true
- : unit = ()
# Current_incr.propagate ()
- : unit = ()
# Term.Executor.run result |> Current_incr.observe
- : string Current_term__.Output.t = Ok "image1"
```

## Static analysis

As well as an incremental result value, as described above, a `Term.t` also includes a "static" component. `Term.t` is defined like this:

```
type 'a t = {
  id : Id.t;
  bind : bind_context;
  ty : metadata_ty;
  v : 'a Dyn.t Current_incr.t;
}
```

`v` is the dynamic (incremental) part. The rest is "static" information about the shape of the pipeline, used to generate the diagrams. Note however that the "static" part can contain incremental components too in some cases. For example, a `bind` operation generates new bits of pipeline at runtime depending on its dynamic input.

The combinators in `Term` build up new terms with the correct static and dynamic parts.
For example, `lib_term/current_term.ml` defines the `Term.map` function like this:

```
let map f x =
  let id = Id.mint () in
  node ~id (Map (Term x)) @@ Current_incr.map (Dyn.map ~id f) x.v
```

- The `Map (Term x)` is the static part, saying that the new node is a `Map` operation with input `x`.
- The `Current_incr.map (Dyn.map ~id f) x.v` is the dynamic part, saying that the value is the result
  of applying `f` to the dynamic value part of `x` (`x.v`).
  `Dyn.map` handles the `result` type (only affecting `Ok` values)
  and the `Current_incr.map` handles the incremental part, so it will update automatically.

`map` operations aren't very interesting (they only appear on diagrams if they fail), but primitive operations
and some other combinators are shown. We can render the `result` pipeline above like this:

```
utop # Format.printf "@[<h>%a@]@." Term.Analysis.pp result;;
build result >>= approve
```

You can also use `pp_dot` instead of `pp`, to generate a graphviz dot file.

## OCurrent

The above sections describe the internal libraries that are used by the main OCurrent library (in `lib`).
This extends `Current_term` with primitives that use `Lwt` to run jobs asynchronously, a database
to persist results, support for collecting log files, etc.

Instead of using our `Term` module above, you use `Current`, and instead of writing low-level primitives
like `await_approval` directly, you can use the pre-built `Current.Monitor`, `Current.Var` and `Current_cache`.
And instead of running `Term.Executor.run` manually, `Current.Engine.run` will run an `Lwt` thread that
calls `propagate` in a loop, triggered by `Current.Engine.update`.

Then, various other libraries provide extensions, such as a web interface, Cap'n Proto RPC, GitHub support, etc.
These are in their own packages to keep the number of dependencies of the core `current` package small.
