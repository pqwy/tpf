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

Tpf is closely related to other well-known datatype-generic approaches.
It shares the underlying data model with [SYB][syb], and uses the
[spiny][syb-reloaded] encoding. It shares the manifest representation with
[GHC.Generics][ghc-generics]. It arises as an adaptation of approaches like
these to a language without overloading, which gives it an idiomatic flavor, and
lends it the name.

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

- opam package `tpf` is at the heard of everything.
- opam package `tpf-ext` throws support for various third-party libs into the
  mix:
  - `tpf-ext.fmt` has generic formatters and depends on [`fmt`][fmt].
  - `tpf-ext.sexplib` has generic [`sexplib`][sexplib] conversions.
  - `tpf-ext.cmdliner` has some generic [`cmdliner`][cmdliner] terms.
  - `tpf-ext.qcheck` is generic [`qcheck`][qcheck].
  - `tpf-ext.crowbar` is generic [`crowbar`][crowbar].
  - tpf_json.ml
- opam package `tpf-deriving` contains the generic deriver. It is worth pointing
  out that Tpf was built so that you can also use it without PPX.

[fmt]: https://erratique.ch/software/fmt
[sexplib]: https://github.com/janestreet/sexplib0
[cmdliner]: https://erratique.ch/software/cmdliner
[qcheck]: https://github.com/c-cube/qcheck
[crowbar]: https://github.com/stedolan/crowbar

## Using generic functions

Equip a type with its generic:

```
type ('a, 'b) tweedledum =
| Tw0 of 'a * 'b
| Tw1 of foo
[@@deriving tpf]
```

This derives `data_tweedledum`. You can apply any generic function to it:

```
(* val pp_tweedledum: 'a Fmt.t -> 'b Fmt.t -> ('a, 'b) tweedledum Fmt.t *)

let pp_tweedledum pp_a pp_b =
  Tpf_fmt.data3 data_tweedledum pp_foo pp_a pp_b
```

When ready, you can opt to drop the training wheels:

```
let meta0 = Tpf.variant "Tw0" 0
let meta1 = Tpf.variant "Tw1" 1
let tw0 a b = Tw0 (a, b)
let tw1 foo = Tw1 foo
let data_tweedledum: _ Tpf.data3 =
  { view = (fun qfoo qa qb ->
      V.(function
      | Tw0 (a, b) -> A (A (K tw0, a, qa), b, qb)
      | Tw1 foo -> A (K tw1, foo, qfoo)),
      (function Tw0 _ -> meta0 | Tw1 _ -> meta1))
  ; schema =
    S.(fun qfoo qa qb ->
      [ A (A (K tw1, qa), qb), meta0; A (K tw2, qfoo), meta1 ]) }
```

There are further examples generic representations in
[`Tpf_std.ml`](src/tpf_std.ml).

## Making generic functions

Simple random-thing generator. It doesn't know about depth or variant
distribution!

```
let pick: 'a list -> 'a = ...

module G = Tpf.Generic (struct type 'a q = unit -> 'a end)

(* val g_random: ('a, G.p) schema -> unit -> 'a *)
let rec g_random sch () =
  let rec go: 'a. ('a, _, _) Tpf.S.schema -> 'a = Tpf.S.(function
  | K f -> f
  | A (s, f_a) -> go s G.(!:f_a ())
  | R s -> go s (g_random schema ())) in
  go (pick sch |> schema)

(* This has to be made available. *)
include G.P

(* This is nice to have available. *)
include G.Data (struct type 'a r = unit -> 'q let gfun = g_random end)
```

```
let r_tweedledum r_a r_b = data3 tweedledum r_foo r_a r_b
```

There are further examples of generic functions in [`tpf-ext`](src-ext).

## Performance

### Tpf is glacially slow...

You can write a generic function that, for example, counts the number of
recursive occurrences, and use this function to measure the length of a list.

It takes about 5x the time it takes `List.length`.

Of course it does. `List.length` compiles to a 4-instruction loop. The Tpf
version ends reconstructs the entire list (albeit lazily), and explores every
field of every block in it.

### ... and Tpf is lightning fast

If you use [ppx_sexp_conv][ppx-sexp-conv], the PPX-based Sexplib deriver, and
compare the performance of `sexp_of_tweedledum` functions it generates to the
performance of a Tpf-based generic version, the ratio is about 1.25 (that is,
generic version is 25% slower).

If you compose these conversion functions with something that interacts with the
actual strings, the overall performance hit of using the generic version drops
to just below 10%.

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

### PPX derivers

The use cases of [`ppx-deriving`][ppx-deriving] overlap with those of Tpf, even
if the two approaches are meant to do different things.

- Tpf is generally slower, especially when consuming values.
- Tpf is less flexible, as it can not extend the language.
- Tpf does not care about hygiene.
- Tpf requires no tooling or external support.
- Tpf makes it infinitely easier to write generic functions.

### OCaml SYB

(As seen [here][ocaml-syb].)

- Tpf does not require a patched compiler.
- Tpf is faster because the manifest representations turn out to be.
- Tpf supports creating values, but these could be added to OCaml SYB too.
- Tpf is laid out to be more practically usable. But because at its heart it is
  still a variants of SYB, their expressivity is the same.

### Generic programming in OCaml

(As seen [here][generic-programming-in-ocaml].)

Tpf is *tagless*. It strictly avoids type reflection, which leads to a number of
differences that make Tpf feel more natural to an OCaml programmer.

- Tpf generic functions are parametrically polymorphic.
- Tpf does not essentially depend on PPX extension points.
- Tpf does not import an overloading semantics into your programs.
- Tpf does not hide the fact that a particular function is not implemented at a
  particular type from the type checker.
- Tpf is decidedly minimalist.
  
[ppx-deriving]: https://github.com/ocaml-ppx/ppx_deriving
[ocaml-syb]: https://github.com/yallop/ocaml-syb
[generic-programming-in-ocaml]: https://arxiv.org/pdf/1812.11665.pdf
