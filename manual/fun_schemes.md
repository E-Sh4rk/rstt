# Function schemes

A *function scheme* (module `FunSig`) is a function signature that is not yet a
regular type: besides ordinary types, it may contain **label variables** and
**groups**, i.e. families of bindings replicated over an unknown, finite index set.

A scheme is not a type: it denotes a *set* of regular arrow types, one per
instantiation of its label variables and groups. Before a scheme can be used by
the type checker it must be turned into a regular type, which is the job of

```
FunSig.specialize : t -> Ty.t -> t
```

`specialize sch arg` uses the concrete argument type `arg` of a call site to
instantiate as many of the scheme's variables as the call determines, and
`FunSig.to_regular` then converts the (hopefully fully instantiated) result into
a `Builder.t`. Variables that the call does not determine are deliberately left
as they are: `specialize` never guesses, and `to_regular` is the operation that
finally complains.

The document is organised as follows.

* [Syntax](#syntax) — the concrete syntax of schemes.
* [Label variables](#label-variables) — semantics of `#x`.
* [Groups](#groups) — semantics of indexed families of bindings.
* [Specialization algorithm](#specialization-algorithm) — the
  resolution algorithm.

---

## Syntax

### Signatures

A scheme is parsed by `Rstt_repl.IO.parse_funsig` and must be a *single* arrow
whose domain is an argument list:

```
(x: int, y: lgl) -> CHR
(x: int, ...: any, y: lgl) -> CHR
(x: int, ...: (any, any), y: lgl) -> CHR
```

`...:` introduces the two tails of `Arg.atom` (`pos_tl` and `named_tl`); the
parameters written before it are *positional-or-named* (`pos_named`), the ones
written after it are *name-only* (`named`). When `...:` is omitted, both tails
default to `absent`, so the parameter list is closed.

Anything that the scheme grammar cannot express is simply kept as a regular
`Builder.t` inside the `FRegular` constructor, so the scheme grammar is a strict
superset of the type grammar:

```
(x: {a: int} | {b: lgl}) -> [int, lgl]
```

### Lists (records)

```
{a: int, b: lgl}        (* closed: every other field is absent *)
{a: int, tl}            (* open: every other field has type tl *)
```

The last element of a list, when it is a bare type instead of a `label: type`
binding, is the *tail* of `Lst.atom`, i.e. the type given to every field that is
not explicitly bound. It defaults to `absent`, which makes the list closed.

### Attributes and classes

```
any with {names: chr}
<data.frame> with {row.names: int}
```

`t with a` builds `FAttr {content = t; classes = CAny; attrs = a}`, and
`<...>` builds the `classes` component.

### Label variables

The token `#x` (`SYMID`) denotes a label variable. It may occur:

* as a **list key**: `{#k: 'a}` — the field whose name is the value of `#k`
* as an **argument key**: `(#k: 'a)` — the argument whose name is the value of `#k`
* as a **type**: `#k` — the singleton string type `"…"` whose content is the
  value of `#k`.

```
(a: {#k: 'a}, b: #k) -> 'a
(x: {a: #k}) -> {#k: int}
(x: any with {names: #k}) -> {#k: int}
```

### Groups

A **group** is written as a parenthesised group of bindings followed by an index
name:

```
{ (#r_i: 'a_i)_i, (#r_j: 'a_j)_j }
```

* `( … )_i` is a *repetition* over the group named `i`: at instantiation time it
  is replaced by one copy of its body per instance of `i`.
* Variables belonging to the group are those whose name ends with `_i`:
  `#r_i` is a **label column** of `i`, `'a_i` a **type column** of `i`.
  A group-indexed variable may only occur inside a repetition over its own
  group.
* The same group name used at several places in the scheme denotes the *same*
  index set, with the same column values — this is what lets a scheme relate
  fields of two arguments, or of an argument and the result.

Repetitions are allowed wherever bindings are keyed by a label:

* list bindings: `{ (#r_i: 'a_i)_i, tl }`;
* named parameters: `(x: int, (#p_i: 'a_i)_i, ...: any)`;
* attribute lists: `any with { (#r_i: 'a_i)_i }`.

Repetitions over *positional* parameters are out of scope (they would be indexed
by position, not by label, and are a different problem).

### Well-formedness of a scheme

The following are static (argument-independent) checks:

1. Every group-indexed variable occurs inside a repetition over its group, and
   repetitions are not nested (v1 restriction, see
   [open questions](#open-questions)).
2. Every repetition binds at least one label column of its group as a key, so
   that the instances of a group are identified by labels.
3. Within a single record/parameter list, the labels produced by the constant
   bindings, by the free label variables and by every repetition must be
   pairwise distinct (this is not a check but the very shape of the
   [matching problem](#the-matching-problem)).
4. A group (or label variable) that does not occur in the domain can never be
   determined by `specialize` and should be reported at declaration time.

---

## Label variables

A label variable `#x` stands for one unknown label (equivalently, for one
unknown singleton string). Its two occurrence forms are duals:

* in **key** position (`{#x: t}`, `(#x: t, …)`) it names a field/parameter;
* in **type** position (`b: #x`) it is the singleton string type of that name.

The link between the two is what makes schemes useful: `get: (a: {#k: 'a}, b:
#k) -> 'a` says "the second argument is the name of the field of the first
argument that is returned".

---

## Groups

### Semantics

Fix a scheme `S`. An **instantiation** `θ` of `S` gives:

* for every free label variable `#x`, a label `θ(#x)`;
* for every group `g`, a finite set `θ(g)` of **instances**; for every instance
  `ι ∈ θ(g)` and every label column `#c_g` of `g`, a label `θ(#c_g)(ι)`, and for
  every type column `'a_g`, a type `θ('a_g)(ι)`.

`θ(S)` is the regular signature obtained by replacing each repetition
`(#c₁_g: T₁, …, #cₛ_g: Tₛ)_g` by the concatenation, over `ι ∈ θ(g)`, of the
bindings `θ(#cₜ_g)(ι) : Tₜ[ι]`, where `Tₜ[ι]` is `Tₜ` with every column of `g`
replaced by its value at `ι`.

Two instances of the same group that agree on *all* label columns are
indistinguishable and would produce duplicate labels in any repetition, so we
may — and do — identify an instance with its tuple of label values:

> `θ(g)` is a finite relation `θ(g) ⊆ 𝕃^{Λ(g)}`, where `Λ(g)` is the set of
> label columns of `g` and `𝕃` the set of labels. The type columns are functions
> of that relation.

`θ` is **well-formed** when, at every record/parameter-list occurrence, all the
labels it produces are pairwise distinct. Two consequences are worth naming
separately:

* **injectivity** — a label column used as a key at some occurrence is injective
  on `θ(g)`;
* **disjointness** — the images of two distinct (group, column) pairs occurring
  in the same record are disjoint, and are disjoint from the constant labels and
  from the free label variables of that record.

A free label variable is exactly the degenerate case of a group: one column,
cardinality fixed to `1`. The algorithm below treats both uniformly.

### Examples

```
merge: (x: { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j }, y: { (#r_i : 'a_i)_i, (#r_k : 'a_k)_k }) -> { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j, (#r_k : 'a_k)_k }
rename: (x: { (#o_i : 'a_i)_i }, renaming: { (#o_i : #n_i)_i }) -> { (#n_i : 'a_i) }
test: (#l: #v, field: #l, ...) -> { #v: #l }
```

* **`merge`** — `i` is the set of fields common to `x` and `y`, `j` the fields
  private to `x`, `k` the fields private to `y`. Nothing in the scheme says
  "common" or "private": it follows from the three records being closed
  (so each group triple must *cover* the corresponding argument) and from
  well-formedness (so the three images must be *disjoint*, which is only
  expressed by the result). Note that `i` uses a single type column `'a_i` in
  both `x` and `y`, so the two arguments must agree on the type of a common
  field; using two columns `'a_i` and `'b_i` and returning `'b_i` would give
  "`y` wins" semantics instead.
* **`rename`** — group `i` has two label columns, `#o` (old name) and `#n` (new
  name), and one type column `'a`. The `renaming` argument observes both columns
  at once, which is what ties them together; `x` observes `#o` only, and the
  result is keyed by `#n`. Well-formedness of the result additionally requires
  `#n` to be injective, i.e. the renaming must not collapse two fields.
* **`test`** — no group, but a label variable used as a *parameter name*, whose
  value comes from another parameter. Resolving it requires two passes.

---

## Specialization algorithm

### What `specialize` computes

`specialize` is given a scheme `S` and the concrete argument type `τ`. It does
**not** decide whether the call is well typed — it could not: the types in the
columns may still need to be instantiated, and checking the application is the
job of the ordinary subtyping/tallying machinery at the call site. Its only job
is to pick, for each label variable and each group, the instantiation the
argument points at, and to leave alone the ones it does not point at.

Three consequences, which are what keep the algorithm small:

* **it never fails.** A label of `τ` that no part of `S` can match, or a field
  whose type does not fit, is simply left unmatched; the ill-typedness of the
  call is reported later by the application rule. There is therefore no
  soundness obligation on the labels we commit to, and no need for the state to
  bound the set of valid instantiations from below.
* **the result takes no part in resolution.** It is matched against no concrete
  type, so it is not an occurrence: nothing is deduced from it and it is only
  substituted into at the end. Its well-formedness is still checked, but on the
  finished instantiation — see [§ instantiation](#instantiation) — rather than
  as a constraint driving the choices.
* **it commits, or it abstains.** When the rules below single out one
  instantiation, `specialize` takes it; otherwise it leaves the group untouched,
  so that `specialize` can be run again with more information and `to_regular`
  is the operation that finally reports "unresolved".

### The matching problem

Every record of the scheme — a list, a parameter list, an attribute list —
matched against a concrete *record* is an **occurrence** `o`. A scheme record
matched against a type we cannot see into (a type variable, a type we do not
decompose) is **not an occurrence**: it is not an observation, and it takes no
part in what follows. Write

* `L(o)` for the labels bound by the concrete record at `o`, and `fieldty(o, ℓ)`
  for the type of field `ℓ`;
* `closed(o)` for whether the concrete record at `o` can have no field beyond
  `L(o)`, i.e. whether its own tail is `absent`. Note that this is a property of
  the *argument*, where the `⊥` of `E(o)` below is one of the *scheme*: the two
  are independent. When `¬closed(o)`, `L(o)` is a lower bound on what the record
  has rather than the whole of it;
* `E(o)` for the **entities** of the scheme record at `o`: its constant labels,
  its free label variables in key position, one entry `(g, #c)` per key column of
  each repetition, and a distinguished `⊥` when the record is open (its tail is
  not `absent`).

Specializing `o` is choosing, for every `ℓ ∈ L(o)`, which entity **claims** `ℓ`:

```
a_o : L(o) → E(o)
```

The well-formedness conditions of the previous section are then built into the
shape of the problem instead of being constraints to enforce:

* *disjointness* and *injectivity* — `a_o` is a function, so a label has exactly
  one claimant;
* *coverage* of a closed record — `⊥ ∉ E(o)`, so every concrete label must go to
  a real entity;
* an open record — `⊥ ∈ E(o)` absorbs the labels the scheme does not name.

`a_o` is total on `L(o)`, but it is **not surjective**: an entity may claim
nothing. That happens when its binding is optional (`#k: any?`), the absence of
the field satisfying it, and whenever `¬closed(o)`, where the label it names may
simply be one we do not observe. Reading a claim off every entity is the mistake
to avoid: it forces a field to be present that the scheme says need not be.

The instances of a group are **read off** the assignment rather than bounded from
both sides: each `ℓ` with `a_o(ℓ) = (g, #c)` contributes one instance of `g` with
`#c = ℓ`, whose remaining columns are filled by matching the body of the
repetition against `fieldty(o, ℓ)`. Occurrences of the same group must read off
the same instances.

### The state

Two finite, monotonically shrinking components:

* `V_#c ⊆ 𝕃`, the candidate labels of each label column `#c` — a free label
  variable being a group with one column and one instance;
* `A_o(ℓ) ⊆ E(o)`, the entities that may still claim `ℓ`. Initially `E(o)`,
  minus the constant labels different from `ℓ`; `A_o(ℓ) = {ℓ}` when `ℓ` is itself
  a constant of `o`.

`𝕃` is the set of labels and singleton strings occurring in `τ` or in `S`: a
string appearing as a value in the argument (`{o1: "n1"}`) may become a label in
the result (`{n1: 'a}`).

Type columns are deliberately not part of the state. At instantiation time each
gets one *fresh type variable per instance* and tallying at the call site solves it.

### The loop

```
specialize(S, τ):
  1. static checks; collect the groups, columns and occurrences of S
  2. V_#c := 𝕃 ;  A_o(ℓ) := E(o) minus mismatched constants
  3. repeat
  4.     traverse S against τ, narrowing V and A       (§ narrowing)
  5.     commit the claims singled out by § choosing
  6. until nothing changes
  7. return instantiate(S, V, A)                       (§ instantiation)
```

Step 4 is re-run because a commitment unlocks new positions of the argument: in
`test`, only once `#l = "l"` is known can the parameter *named* `#l` be selected
and its type inspected. Both components only shrink and `𝕃` is finite, so the
loop terminates; in practice it converges in two or three iterations, one per
level of "a label used as a key to reach another label".

### Narrowing

The traversal is the existing `match_ty` / `match_field` / `match_param` walk.
Besides recursing on constant keys, on `FAttr` and on committed claims, it
applies three rules.

* **type position** — a column `#c` matched against a concrete type `ty` gives
  `V_#c ← V_#c ∩ strings_of_ty(pos, ty)`.
* **agreement** — a column is a key at possibly several occurrences, and must
  claim the same labels at each. Where the occurrence settles what it claims —
  `closed(o)`, and the binding of `#c` at `o` mandatory — that is
  `V_#c ← ⋂ { ℓ ∈ L(o) | (g,#c) ∈ A_o(ℓ) }` over every such occurrence; dually
  `(g,#c)` leaves `A_o(ℓ)` as soon as `ℓ ∉ V_#c`. This one rule is what makes
  group `i` of `merge` the *common* fields of the two arguments.

  Otherwise `#c` may be claiming nothing at `o`, or a label outside `L(o)`, and
  intersecting with `L(o)` would be unsound. The occurrence then only *rules
  out* the labels the record does have and that `#c` cannot claim:
  `V_#c ← V_#c \ { ℓ ∈ L(o) | (g,#c) ∉ A_o(ℓ) }` — a subtraction, so a column
  still unconstrained stays so. Both halves are needed in practice: without the
  optional case, a scheme that *adds* a field could never be specialized;
  without the `¬closed(o)` case, one could only ever read a field the argument
  is *known* to carry. R's `$<-` and `attr` are the two:

  ```
  ($<-) : (x: {#k: any?, `r}, k: #k, v: 'b) -> {#k: 'b, `r}
  @({a: int}, "b", v: ff)        #k = "b", a label the argument does not have

  attr  : (x: any with {#k: 'b, any}, which: #k) -> 'b
  @(any with {match.length: int, any}, "capture.start")
                                 #k = "capture.start", not among the observed
  ```

* **exclusion** — a committed claim `a_o(ℓ) = e` removes `e` from `A_o(ℓ')` for
  every other `ℓ'`, *provided* `e` is single-instance: a constant label or a free
  label variable. A group column claims one label per instance and `⊥` claims any
  number, so neither is excluded.

A union of `Arg` or `Lst` atoms is narrowed against each atom separately and the
resulting states are *joined*.

No rule inspects a type except through `strings_of_ty`, which reads the
singleton strings off it: **resolution performs no subtyping test**. The price is
that two repetitions of the same record cannot be told apart by the type of their
body — `(x: {(#r_i: int)_i, (#r_j: chr)_j})` applied to `{a: int, b: chr}` is an
ambiguous cover and stays unresolved — and that a repetition in an open record
claims every label it can reach instead of leaving the ill-typed ones to the
tail. Both are schemes in which the *role* of a field is decided by its type
rather than by its name, and both degrade to an unresolved or a rejected call.

### Choosing

A claim is committed in two cases:

* **sole claimant** — `A_o(ℓ) = {e}`;
* **pinned column** — `V_#c = {ℓ}` with `ℓ ∈ L(o)`, for a *single-instance*
  column `#c`, i.e. a free label variable. Its binding exists and its label is
  known, so it is the entity taking `ℓ`. A group column is not pinned this way:
  a group may have no instance at all. This is what stops a repetition sharing
  the record — or the tail `⊥` — from taking a label that a known label variable
  already names.

Propagating the two settles `get`, `rename`, `test`, and every record in which no
two entities compete for the same label.

When entities do compete, the exact question — who claims what, subject to
coverage and disjointness — is an *exact cover* instance, hence NP-hard, and we
do not try to solve it. Since the algorithm is allowed to be merely plausible,
one deterministic preference is enough:

> **the most constrained claimant wins.** Among `A_o(ℓ) \ {⊥}`, claim `ℓ` for the
> entity whose column has the fewest candidates `|V_#c|`, ties broken in favour
> of the column that is a key at the most occurrences. If entities are still
> tied, commit nothing: the group stays unresolved.

This is exactly what `merge` needs. Group `i` is a key in both arguments, so
*agreement* has already reduced `V_#r_i` to their common labels, which makes it
strictly more constrained than the private groups `j` and `k` — "a field held by
both arguments is a shared field" falls out of the preference instead of being
special-cased. Committing, re-narrowing and committing again then settles the
private fields by *exclusion*.

The preference is a choice, not a deduction, and it can be wrong. What it cannot
do is produce an unsound signature: a bad choice yields a specialized signature
that the application rule rejects, and being rejected is what an ill-typed call
deserves anyway.

### Instantiation

A group `g` is **determined** when every occurrence at which one of its columns
is a key has a committed claimant for each of its labels, and the instances read
off at those occurrences agree. `θ(g)` is that set of instances. A group with no
key occurrence in the domain can never be determined — this is the declaration
time check of [well-formedness](#well-formedness-of-a-scheme).

For each determined `g`:

* order `θ(g)` deterministically — by the order in which the key labels appear
  in the concrete argument, then lexicographically — so that the produced
  signature does not depend on internal hash order;
* allocate, **once for the whole scheme**, a fresh type variable `'a^ι` for every
  type column `'a_g` and every instance `ι`. Sharing these across occurrences is
  essential: it is what makes `merge`'s result field `s` have the same type as
  `x`'s and `y`'s, and `rename`'s result field `n1` the type of `x`'s `o1`;
* replace every repetition over `g` by the concatenation of its body over `θ(g)`,
  substituting `θ(#c_g)(ι)` for label columns (as a label in key position, as a
  singleton string type in type position) and `'a^ι` for type columns.

Expanding a record must not produce the same label twice — two groups that share
a record claiming the same label, or a key column that is not injective. Only
entities sharing a scheme record that is *not* an occurrence can collide this
way, since within an occurrence `a_o` is a function; in practice that means the
result. This cannot be ruled out while resolving (a group may legitimately be empty, so the
*possibility* of a clash never narrows anything), so it is checked here, on the
finished instantiation; the groups involved in a clash are put back to
undetermined.

Undetermined groups and label variables are left untouched, so a partially
specialized signature can be specialized again — the existing
`partially specialized signatures` test relies on this.

### Worked examples

#### `test` — iteration

```
test: (#l: #v, field: #l, ...) -> { #v: #l }
@(l: "v", field: "l")

E(toplevel) = { #l, field, ⊥ }     (open parameter list)

iteration 1   field:  matched by the constant `field`; its body #l against "l"
                      → V_#l = {l}
              l:      A = {#l, ⊥}; setting ⊥ aside, #l is the only claimant, so
                      it claims l — which agreement confirms. But #l was not yet
                      known when the parameter was reached, so its body is not
                      matched this round
iteration 2   #l is pinned to "l" → the parameter named l is selected
              l:      its body #v against "v" → V_#v = {v}
iteration 3   stable

result: (l: "v", field: "l", ...: any) -> {v: "l"}
```

#### `rename` — a two-column group

```
rename: (x: { (#o_i : 'a_i)_i }, renaming: { (#o_i : #n_i)_i }) -> { (#n_i : 'a_i) }
@(x: 'b, renaming: { o1: "n1", o2: "n2" })

x:          concrete type is a type variable → not an occurrence, says nothing
renaming:   L = {o1, o2},  E = { (i,#o) }  (closed, no ⊥)
            → both labels have a single claimant; θ(i) reads off two instances,
              whose column #n is filled by matching the body #n_i against
              "n1" and "n2"  →  θ(i) = { (o1,n1), (o2,n2) }
result:     a_result is a function on column #n → n1 ≠ n2 ✓ (the renaming must
            not collapse two fields)

result: (x: {o1: 'a¹, o2: 'a²}, renaming: {o1: "n1", o2: "n2"}) -> {n1: 'a¹, n2: 'a²}
```

#### `merge` — agreement and preference

```
merge: (x: { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j }, y: { (#r_i : 'a_i)_i, (#r_k : 'a_k)_k }) -> { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j, (#r_k : 'a_k)_k }
@(x: { x: 1, s: 2 }, y: { y: 3, s: 4 })

E(x) = { (i,#r), (j,#r) }     E(y) = { (i,#r), (k,#r) }     (both closed)

`closed(x)` and `closed(y)` are what license the intersections below: each
argument is exactly the two fields shown, so a column that claims nothing there
claims nothing at all.

agreement   #r_i is a key at x and y  →  V_#r_i = {x,s} ∩ {y,s} = {s}
            #r_j only at x → V_#r_j = {x,s} ;  #r_k only at y → V_#r_k = {y,s}
            x ∉ V_#r_i  →  A_x(x) = { (j,#r) }  →  x claimed by j
            y ∉ V_#r_i  →  A_y(y) = { (k,#r) }  →  y claimed by k
choosing    A_x(s) = { (i,#r), (j,#r) } : |V_#r_i| = 1 < |V_#r_j| = 2
            →  s claimed by i, at x and (same reasoning) at y

θ(i) = {s}   θ(j) = {x}   θ(k) = {y}

result: (x: {s: 'aⁱˢ, x: 'aʲˣ}, y: {s: 'aⁱˢ, y: 'aᵏʸ}) -> {s: 'aⁱˢ, x: 'aʲˣ, y: 'aᵏʸ}
```

The result record takes no part in this: `s` is settled by the *preference*, not
by the fact that `i`, `j` and `k` must produce distinct labels there. That fact
is a check on the answer, applied once the instantiation is built.

### Cost and safeguards

* **Termination** — `V` only shrinks, `A` only shrinks, and `𝕃` is fixed before
  the loop; two or three iterations in practice.
* **No blow-up** — instances are read off the assignment, never enumerated, so
  there is no cartesian product over the columns of a group to cap. One
  iteration costs the traversal plus `O(Σ_o |L(o)| · |E(o)|)`.
* **No subtyping in the loop** — resolution reads labels and singleton strings
  off the argument and nothing else; the type machinery runs once, afterwards,
  on the specialized signature.
* **Common case** — a scheme with no group and no competing entity is settled by
  the *type position* rule alone, in one or two traversals.

### Representation notes

A repetition body is restricted to a *single* binding, so a repetition is still
a `(label, ty)` pair and fits the existing `Lst.atom` / `Arg.atom` shapes:

```ocaml
type gvar = { group: string ; column: string }

type label =
| LConst of string
| LVar of string
| LGroup of gvar                (* new: the key of a repetition *)

type ('v,'r,'i) ty =
| FLVar of string
| FGVar of gvar                 (* new: #n_i in type position *)
| ...
```

Nothing is added to the signature itself: the groups, their label columns and
their type columns are all read off the syntax. A group's label columns are the
`LGroup`/`FGVar` nodes mentioning it, and its type columns are the ordinary
variables whose name ends with `_g` — the same convention the surface syntax
uses — which is all `instantiate` needs to freshen them per instance.

Multi-binding bodies (`(#a_i: 'x_i, #b_i: 'y_i)_i`) would require a dedicated
binding constructor and are left for later; the algorithm above is written for
the general case, so only the representation would change.

### Open questions

1. **Nested repetitions** (`((#a_ij: 't_ij)_j)_i`) — the inner index set would be
   per outer instance, i.e. `θ(j)` becomes a function of `ι ∈ θ(i)`, so a
   label is claimed by a *pair* of instances and `a_o` no longer has a fixed
   codomain. Forbidden in v1.
2. **Ordering** — should the expanded fields keep the order of the concrete
   argument? It matters for `Arg` (positional-or-named parameters) more than for
   `Lst`.
3. **Optional bindings inside a repetition** (`(#r_i: 'a_i?)_i`) — a claim
   would no longer imply the field is really there; probably worth forbidding.
   Outside a repetition an optional binding is *not* a problem and is relied
   upon — it is how a scheme adds a field — the agreement rule simply not
   reading a claim off it; but a repetition reads its instances off the claims,
   so an unclaimed column there would have no instance to contribute.
4. **Is the "most constrained claimant" preference the right one?** It is what
   `merge` needs and it is predictable, but it is a guess. The alternatives are
   to abstain on every competition (more unresolved schemes, never a wrong one)
   or to search for real.
