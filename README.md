# RSTT

Work-in-progress set-theoretic type algebra for the R language.

You can test it online, on the web version of the REPL:  
https://e-sh4rk.github.io/rstt/

## Type syntax and constructors

A type denotes a set of R (or C, at the FFI boundary) values. Constructors build up
types describing atomic vectors, lists, tuples, function signatures, attributes/classes,
recursive structures, and the low-level C values that appear at R's C API boundary.
All the examples below can executed in the REPL ([web version](https://e-sh4rk.github.io/rstt/)).

### The type algebra

At the top of the algebra sit two special types and the usual boolean combinators:

| Syntax | Meaning |
|---|---|
| `any` | the type of every value |
| `empty` | the type with no value (bottom type) |
| `T1 \| T2` | union |
| `T1 & T2` | intersection |
| `T1 \ T2` | difference |
| `~T` | negation (shorthand for `any \ T`) |
| `(T)` | grouping |

```
> any ;;
any
> vec \ lgl ;;
CHR | CLX | DBL | INT
```
(`vec`, seen above, is described in [Atomic vectors and primitive values](#atomic-vectors-and-primitive-values) below.)

There is also a `dyn` keyword reserved for a gradual/dynamic type; it is
accepted by the parser but not supported by the REPL.

### Type variables

Two families of identifiers stand for open type variables, and the *case of their
first letter* controls whether the REPL's constraint solver (see
[Subtyping and constraint solving](#subtyping-and-constraint-solving)) is allowed to
instantiate them:

* `` 'a ``, `` 'x ``, ... (starting with a lowercase letter) — **monomorphic**
  variables. They are left untouched by tallying and simply appear as-is in the
  printed result.
* `` 'A ``, `` 'X ``, ... (starting with an uppercase letter) — **polymorphic**
  variables. Tallying treats them as unknowns to solve for.

The same convention applies to *row variables*, written with a backtick instead of a
quote: `` `r `` is monomorphic, `` `R `` is polymorphic (row variables show up inside
list, argument and class types — see below).

### Atomic vectors and primitive values

R's atomic vectors (logical, integer, double, complex, character, raw) are described
with a small family of shorthands, all of which are sugar for an explicit vector
constructor `v(...)` / `v1(...)`:

| Syntax | Meaning |
|---|---|
| `LGL`, `CHR`, `INT`, `DBL`, `CLX`, `RAW` | vector (any length) of that mode |
| `NUM` | vector of `INT` or `DBL` elements that represent integers |
| `vec` | vector of any mode (does not include lists) |
| `lgl`, `chr`, `int`, `dbl`, `clx`, `raw` | same, but *including every "smaller" mode* |
| `X1` (e.g. `INT1`, `LGL1`, ...) | same as `X`, restricted to length exactly 1 (a scalar) |

The lowercase shorthands mirror the coercion order R uses when combining vectors
(e.g. with `c()`): `raw < lgl < int < dbl < clx`, and `chr` sits above everything.
So `int` is not just "an integer vector", it is "an integer vector, or anything that
coerces up to it" — i.e. `raw`, `lgl` or `int`:

```
> RAW | LGL ;;
lgl
> RAW | LGL | INT ;;
int
> vec \ CHR ;;
clx
```

Behind these shorthands is the general vector constructor `v(P)` (any length) /
`v1(P)` (length exactly 1), which takes a *primitive-value* expression `P`. Primitive
values support the same `| & \ ~` algebra, plus:

* literals: `42L` (int), `42.` (double), `42` (int or double), `"txt"` (char), `tt` / `ff` (logical), and
  intervals such as `(1L..10L)`, `(..)`, `1L..`, `..10L`;
* primitive variables `'a` (to capture genericity);
* `^P`, which excludes R's `NA` marker from `P` (every primitive domain implicitly
  contains `NA` unless you subtract it with `^`).

A literal used directly as a type is shorthand for the length-1 vector containing
exactly that value (`42L` means `v1(42L)`).

```
> 42L = v1(^42L) ;;
true
> v('a) -> v1('a) ;;
vec & v('a) -> vec1 & v1('a)
```
(the last example is the type of a function that accepts any atomic vector and return a scalar from this vector)

### Tuples

`[T1, T2, ..., Tn]` is a fixed-size, pointwise-covariant tuple; `[]` is the empty
tuple (printed `tuple0`):

```
> [] ;;
tuple0
> [int,lgl] ;;
[int, lgl]
```

Note that tuples do not directly exist in R ([lists](#lists) or [arguments](#arguments) are used instead), but are included in the type algebra for convenience (they can be used to encore other data structures, for instance arguments of C functions from libraries).

### Lists

`{ ... }` describes R's `list()` values — an unordered collection of named and/or
unnamed elements:

* `{ name1: T1, name2: T2 }` gives named element `name1` the type `T1` and named element `name2` the type `T2`. By default a list type is *closed*: exactly the listed fields may be present, nothing else.
* Add a trailing, unlabelled type to make it *open*: `{ lbl: T, U }` says any extra
  element (named or not) must have type `U`. A lone `{ U }` is a homogeneous list of
  any number of `U`-typed elements — `{ any }` is the top list type.
* A field type ending in `?` marks that field optional (may be absent), see
  [Arguments](#arguments) for a worked example.
* `` { name1: T1, `r } `` uses a row variable `` `r `` to capture the type of any named element other than `name1` —
  handy for describing list transformers that only touch specific fields.

```
> { a:42L, any } ;;
{ a: 42L, any }
> { a:ff } ;;
{ a: ff }
> { a:42L, b:ff, int } <= { int } ;;
false
> { a:42L, b:ff, int } <= { b:ff, int } ;;
true
> { a:absent, `r } -> { a:v1(^42L), `r } ;;
{ a: absent, `r } -> { a: 42L, `r }
```
(the last example is the type of a function that sets named element `a` to `42` on any list
that does not contain an element `a` yet, while
leaving every other field untouched, thanks to the row variable `` `r ``.)

### Arguments

R's calling convention has two sides — how a function is *defined* and how it is
*called* — and RSTT has one constructor for each:

* `(lbl1: T1, lbl2: T2, ...: Trest)` is a **definition-site** signature: a fixed
  set of *named* formal parameters, plus an optional `...: T` catch-all for extra
  arguments (R's `...`). Append `?` to a parameter's type to make it
  optional.
* `@(T1, T2, lbl: Tn, ...: Trest)` is a **call-site** signature: a mix of
  purely positional argument types and named ones, with the same optional `...`
  catch-all.

Checking whether a call-site type is a subtype of a definition-site type tells you
whether that call actually matches that signature:

```
> @(int, b:lgl) <= (a:int, b:vec) ;;
true
> @(int, chr, b:lgl) <= (a:int, ...:vec) ;;
true
```

### Arrows

`T1 -> T2` is the type of an R closure from `T1` to `T2` (`T1` is typically one of
the argument types above). Because R closures can themselves carry attributes and a
class, `->` implicitly allows arbitrary attributes/classes on the function value
itself. `T1 --> T2` is the "bare" version of the same arrow, without that implicit
wrapping — mostly useful as a building block when typing C functions from libaries:

```
> int --> int ;;
int --> int
> int -> int ;;
int -> int
```

Intersections of arrows model overloaded/case-dispatching functions:

```
> (int1 -> c_true) & (int -> c_bool) & (int\int1 | vec\int -> c_false) ;;
(int1 -> c_true) & (vec & ~int1 -> c_false)
```

### Classes and attributes

Most R values can carry a `class` attribute (used for S3/S4 dispatch) plus arbitrary
other attributes (`dim`, `names`, ...). RSTT tracks these with two constructors:

`<...>` restricts the *classes* a value carries:

| Syntax | Meaning |
|---|---|
| `<class1>` | has exactly class `class1`, nothing else |
| `<class1, ...>` | has (at least) `class1`; other classes are unspecified |
| `<~class1, ...>` | does **not** have `class1`; other classes are unspecified |
| `<?class1, ...>` | `class1` may or may not be present |

```
> <class1> ;;
<class1>
> <class1, ...> ;;
<class1, ...>
> <class1,~class2,?class3, ...> ;;
<class1, ~class2, ...>
```
(`class3` disappears from the last result: as every *other* class is
unspecified via `...`, asserting `class1` *may be present* on top is redundant, so the
simplifier drops it.)

`with` attaches a type for the rest of the attributes (as a list, using the same
`{...}` syntax as above):

```
> int with { dim:int } ;;
int with { dim: int }
> int <= int with { any } ;;
true
```
(the last example shows that a bare vector type like `int` already means "`int`, with any
attributes/classes whatsoever" — `with`/`<...>` only become necessary once you want
to *constrain* them, e.g. down to no attributes at all with `T<> with {}`.)

### `externalptr`, `env`, `sym`, `lang`, `null`

A handful of remaining base R SEXP types, plus a parametrised external pointer:

```
> int|null ;;
int | null
> externalptr(int) ;;
externalptr(int)
```

### Recursive types

`T where x = Tx [and y = Ty ...]` binds one or more recursive type variables (using a
plain, un-quoted identifier) that may refer to themselves inside their own
definition:

```
> a where a = { a } | dbl ;;
x1 where x1 = dbl | { x1 }
> a where a = { a } | int <= a where a = { a } | dbl ;;
true
```

### C-level values

At the C API boundary (`.Call`/`.External`), values may have a different, lower-level
representation than R SEXPs — e.g. a C `int` is not a vector, and uses a sentinel value for `NA` instead
of a tag. A separate family of constructors covers this domain:

| Syntax | Meaning |
|---|---|
| `c_int`, `c_double`, `c_char`, `c_void`, `c_string`, `c_null`, `c_ptr` | C base types |
| `c_bool`, `c_true`, `c_false` | C `int` used as a boolean |
| `c_int_na` | `c_int`, including the NA sentinel |
| `c(42)`, `c("txt")` | singleton C values |
| `*T` | (possibly null) pointer to `T` |

```
> c_null ;;
c_null
> c_int \ c(42) ;;
c(..41) | c(43..)
> *c_int ;;
*c_int
```

## Using the REPL

Every REPL command is a *type expression* terminated with `;;`. A command can be:

* a bare type, printed as-is:
  ```
  > INT | LGL ;;
  INT | LGL
  ```
* prefixed with a name in quotes, which is echoed back with the result (handy to
  label a series of experiments, as `tests/tests.txt` does throughout):
  ```
  > "named": INT | LGL ;;
  named: INT | LGL
  ```
* suffixed with `::` (instead of `:`) to print the *raw*, internal encoding of the
  type instead of the friendly R-oriented syntax — mostly useful when debugging the
  type builder itself:
  ```
  > "raw-type":: tt ;;
  raw-type: attr({ _c : v({ _e : prim(lgl(tt)) }) ; _class : class({ ;; ff | tt }) ; _attr : lst(record) ..})
  ```

The REPL also accepts instructions `type name = T ;;` to define a type alias
(`name` will denote the type `T` in future prompts).

## Subtyping and constraint solving

Beyond building types, the REPL can check subtyping and solve constraints between
types that contain (polymorphic) type/row variables.

### Comparisons

`T1 <= T2`, `T1 >= T2` and `T1 = T2` check subtyping/equivalence and print a boolean:

```
> RAW1|LGL1|INT1|DBL1|CLX1|CHR1 = vec1 ;;
true
> a where a = { a } | dbl <= a where a = { a } | int ;;
false
```

### Tallying

`[[ T1 op T2 ; T3 op T4 ; ... ]]` (with `op` one of `<=`, `>=`, `=`) solves the given
constraints for every *polymorphic* variable (uppercase-first `` 'A ``/`` `R ``, see
[Type variables](#type-variables)) they contain, and prints the resulting
substitution(s):

```
> [[ 'A <= INT ; 'A >= 42L ]] ;;
[[
   'A: 42L | INT & 'A
]]
```

A constraint set can be satisfiable in more than one way; tallying then prints one
`[[ ... ]]` block per alternative substitution.

### Explicit substitutions and application

`[[ v1: T1 ; v2: T2 ; ... ]]` builds a substitution directly (rather than solving for
one), mapping variable `v1` to `T1`, etc. Writing a type (or another substitution)
right after a substitution or a tally *applies* it — this is how you plug a solved
constraint back into a type:

```
> 'a [[ 'a: LGL ]] ;;
LGL
> <class1,`C> [[ <class1,class2> <= <class1,`C> ]] ;;
<class1, class2>
```

## License

This software is distributed under the MIT license.
See [`LICENSE`](LICENSE) for more info.  
*This work is funded by the ERC CZ LL2325 grant.*
