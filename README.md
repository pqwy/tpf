# Tpf — minimalist datatype-generic programming

%%VERSION%%

Tagless/trivial polytypic functions (Tpf) is a library for datatype-generic
programming in OCaml. It aims to be simple and idiomatic.

**Generic** programming is all about not writing the same old `equal` again.

**Datatype-generic** (or polytypic) programming is solving this at the language
level.

Tpf allows you to write functions that work on a wide range of unrelated data
types. Such functions require only the base language, need no runtime, and
seamlessly interoperate with the rest of OCaml. Tpf works in the same stage as
the rest of your program, and doesn't rely on meta-programming.

Tpf is closely related to other well-known datatype-generic approaches.  It
shares the underlying data model with [SYB][syb], and uses the
[spiny][syb-reloaded] encoding. This is a manifest representation, like the one
[GHC.Generics][ghc-generics] use. It arises as an adaptation of approaches like
these to a language without overloading, giving it an idiomatic flavor, and
lending the name.

Tpf has no dependencies and is distributed under the ISC license.

[syb]: https://wiki.haskell.org/Scrap_your_boilerplate
[syb-reloaded]: https://www.cs.ox.ac.uk/bruno.oliveira/SYB0.pdf
[ghc-generics]: https://wiki.haskell.org/GHC.Generics

## Installation

Tpf can be installed with `opam`:

    opam install tpf
    opam install tpf-ext       # Install the optional support for third-party libs
    opam install tpf-deriving  # Install the optional PPX deriver

If you don't use `opam` consult the [`tpf.opam`](tpf.opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated by `odoc` from
the interfaces. It can be consulted [online][doc].

[doc]: https://pqwy.github.io/tpf/doc

## Library layout

Tpf contains several optional libraries and sub-libraries:

- opam package `tpf` contains the core library:
- opam package `tpf-ext` adds support for various third-party libs:
  - `tpf-ext.fmt` has generic formatters and depends on [`fmt`][fmt].
  - `tpf-ext.sexplib` has generic [`sexplib`][sexplib] conversions.
  - `tpf-ext.cmdliner` has some generic [`cmdliner`][cmdliner] terms.
  - `tpf-ext.qcheck` is generic [`qcheck`][qcheck].
  - `tpf-ext.crowbar` is generic [`crowbar`][crowbar].
- opam package `tpf-deriving` contains the generic deriver. It is worth pointing
  out that Tpf can be used without PPX.

[fmt]: https://erratique.ch/software/fmt
[sexplib]: https://github.com/janestreet/sexplib0
[cmdliner]: https://erratique.ch/software/cmdliner
[qcheck]: https://github.com/c-cube/qcheck
[crowbar]: https://github.com/stedolan/crowbar

## Using generic functions

Equip a type with its *generic*:
```
type ('a, 'b) tweedledum =
| Tw0 of 'a * int
| Tw1 of 'b
[@@deriving tpf]
```

This derives `data_tweedledum`, the generic representation of `tweedledum`.
That's the key to applying generic functions to `tweedledum`:
```
(* type 'a eq = 'a -> 'b -> bool
   val eq_tweedledum: 'a eq -> 'b eq -> ('a, 'b) tweedledum eq *)

let eq_tweedledum eq_a eq_b =
  Tpf_std.Eq.data3 data_tweedledum (=) eq_a eq_b
```
```
(* type 'a fmt = Format.formatter -> 'a -> unit
   val pp_tweedledum: 'a fmt -> 'b fmt -> ('a, 'b) tweedledum fmt *)

let pp_tweedledum pp_a pp_b =
  Tpf_fmt.data3 data_tweedledum Fmt.int pp_a pp_b
```

To *instantiate* a generic function at a given type, you need to supply its
`data`, and one value per type it references — the *query* — that determines
what to do at these other types.

You can also opt to drop the training wheels:
```
let meta0 = Tpf.variant 0 "Tw0"
let meta1 = Tpf.variant 1 "Tw1"
let tw0 a x = Tw0 (a, x)
let tw1 b = Tw1 b
let data_tweedledum: _ Tpf.data3 =
  { view = (fun qint qa qb ->
      Tpf.V.(function
      | Tw0 (a, x) -> A (A (K tw0, a, qa), x, qint)
      | Tw1 b -> A (K tw1, b, qb)),
      (function Tw0 _ -> meta0 | Tw1 _ -> meta1))
  ; schema = Tpf.S.(fun qint qa qb ->
      [ A (A (K tw0, qa), qint), meta0; A (K tw1, qb), meta1 ]) }
```

There are further examples of generics in [`Tpf_std`](src/tpf_std.ml).

## Writing generic functions

These come in two flavors: consumers and producers.

Iterators, for instance, are consumers. Consumers work on a `Tpf.view`:
```
module G = Tpf.Generic (struct type 'a q = 'a -> unit end)
open Tpf
open G

(* val g_iter: ('a, G.p) view -> 'a -> unit *)

let rec g_iter view x =
  let rec go: 'a. ('a, _, _) V.spine -> unit = V.(function
  | K _ -> ()
  | A (s, a, f_a) -> go s; !:f_a a
  | R (s, a) -> go s; g_iter view a) in
  go (spine view x)
```

While random generators are producers. Producers work on a `Tpf.schema`:
```
let pick: 'a list -> 'a = ...

module G = Generic (struct type 'a q = unit -> 'a end)
open Tpf
open G

(* val g_random: ('a, G.p) schema -> unit -> 'a *)

let rec g_random schema () =
  let rec go: 'a. ('a, _, _) S.spine -> 'a = S.(function
  | K f -> f
  | A (s, f_a) -> go s G.(!:f_a ())
  | R s -> go s (g_random schema ())) in
  go (pick schema |> spine)
```

There are further examples of generic functions in [`Tpf_std`](src/tpf_std.ml)
and [`tpf-ext`](src-ext).

## Performance

### Tpf is glacially slow...

You can write a generic function that, for example, counts the number of
recursive occurrences, and use this function to measure the length of a list.

It takes about 5x the time it takes `List.length`.

Of course it does. `List.length` compiles to a 4-instruction loop. The Tpf
version reconstructs the entire list (albeit lazily), and explores every field
of every block in it.

### ... and Tpf is lightning fast

If you use the industry-leading PPX-based Sexplib deriver,
[ppx_sexp_conv][ppx-sexp-conv], and compare the performance of derived
conversion functions (`sexp_of_tweedledum` / `tweedledum_of_sexp`) to the
performance of a Tpf-based generic version, the ratio is about 1.25. Generic
version is 25% slower.

If you compose these conversion functions with something that interacts with the
actual bytes, the overall performance hit of using generics drops to just below
10%.

Incidentally, most attempts at writing a generic printer will end up being
*faster* than hand-written printers, as `CamlinternalFormat.make_printf` is
a formidable opponent.

[ppx-sexp-conv]: https://github.com/janestreet/ppx_sexp_conv

### Tpf is usable.

Clearly, whether the overhead introduced by generic functions is *significant*
depends entirely on what they are doing. And whether this ends up being
*prohibitive*, depends entirely on your use-case.

It doesn't cost much to try.

## Compared to alternatives

### PPX deriving

The use cases of [ppx_deriving][ppx_deriving] overlap with those of Tpf, even if
the two approaches are meant to do different things.

- Tpf is generally slower, especially when consuming values.
- Tpf is less flexible, as it can not extend the language.
- Tpf does not care about hygiene.
- Tpf requires no tooling or external support.
- Tpf makes it infinitely easier to write generic functions.

### OCaml SYB

(As seen [here][ocaml-syb].)

- Tpf does not require a patched compiler.
- Tpf has no concept of overloading, so the user is responsible for manually
  specifying what to do at contained types.
- Tpf is faster because the manifest representations and internalized recursion
  turn out to be.
- Tpf is typically easier to write generic functions for. But their
  expressivity is still exactly the same.

### Generic programming in OCaml

(As seen [here][generic-programming-in-ocaml].)

Tpf is *tagless*. It strictly avoids type reflection, which leads to a number of
differences that make Tpf feel more natural to an OCaml programmer.

- Tpf generic functions are parametrically polymorphic.
- Tpf does not essentially depend on PPX extension points.
- Tpf does not import its private overloading semantics into your programs.
- Tpf does not hide the fact that a particular function is not implemented at a
  particular type from the type checker.
- Tpf is decidedly minimalist.
  
[ppx_deriving]: https://github.com/ocaml-ppx/ppx_deriving
[ocaml-syb]: https://github.com/yallop/ocaml-syb
[generic-programming-in-ocaml]: https://arxiv.org/pdf/1812.11665.pdf
