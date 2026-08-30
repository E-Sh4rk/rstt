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
* [Label variables](#label-variables) — semantics of `#x`, and what the current
  implementation resolves.
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

The token `#x` (`SYMID`) denotes a label variable. It may currently occur:

* as a **list key**: `{#k: 'a}` — the field whose name is the value of `#k`;
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
   pairwise distinct (this is a *constraint*, not a check — see
   [the constraint language](#the-constraint-language)).
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

The current implementation (`FunSig.specialize`) resolves them with a single
top-down pass:

* it walks the scheme's domain against the concrete argument type;
* whenever a `FLVar x` is matched against a concrete type, `strings_of_ty`
  computes a finite over-approximating set of strings for it, and this set is
  intersected into the environment entry for `x`;
* several atoms of the argument (a union of `Arg` atoms) are matched
  independently and their constraints are *joined* (union of the string sets);
* a variable is instantiated only when its set is a singleton; an empty set is
  an error;
* positions that cannot be analysed are silently skipped — in particular
  **a key `#x` whose value is not yet known cannot be used to select a field**,
  so `(#l: #v, field: #l, …)` cannot be resolved today.

That last point is the first motivation for making the algorithm iterative.

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

### Specification

`specialize` is given a scheme `S`, the concrete argument type `τ`, and
optionally an expected result type `ρ` (default `any`). Let

```
Θ = { θ | θ(S) is well-formed, τ ≤ dom(θ(S)) and ret(θ(S)) ≤ ρ }
```

i.e. the instantiations under which the call is well typed. Then:

* if `Θ = ∅`, the call cannot be typed: **fail**;
* otherwise, instantiate a variable (resp. a group) exactly when **all `θ ∈ Θ`
  agree on it**, and leave the others untouched.

This is the exact generalisation of the current rule "resolve a label variable
only when a single string can be matched with it".

`Θ` is not computable in general (subtyping in the presence of the unknown
cardinalities, plus arbitrary types in the columns), so the algorithm computes a
**superset** `Θ̂ ⊇ Θ` and takes what is common to all of `Θ̂`. Both directions of
the specification stay sound:

* a literal true in all of `Θ̂` is true in all of `Θ`, so what we commit to is
  correct;
* `Θ̂ = ∅` implies `Θ = ∅`, so a reported failure is a real failure.

The price of the over-approximation is that some resolvable variables stay
unresolved — the same "best effort" behaviour as today.

### Shape of the algorithm

Three layers, wrapped in an outer fixpoint:

```
specialize(S, τ, ρ):
  1.  static checks; collect the groups, columns and variables of S
  2.  𝕃 := labels-and-strings(τ) ∪ labels-and-strings(ρ) ∪ constants(S)
  3.  σ := ⊤                                  -- no information
  4.  repeat
  5.      C := generate(S, τ, ρ, σ)            -- structural traversal (§ generation)
  6.      σ' := σ ⊓ propagate(C)               -- inner fixpoint  (§ propagation)
  7.      σ' := σ' ⊓ complete(C, σ')           -- backbone        (§ completion)
  8.      if σ' = σ then break else σ := σ'
  9.  if some domain is empty or C is unsatisfiable then fail
  10. return instantiate(S, σ)                 -- (§ instantiation)
```

Step 5 depends on `σ` because knowing a label unlocks new positions of the
argument (`test`: once `#l = "l"` is known, the parameter named `#l` can be
matched). Step 6/7 depend on `C`. Both `generate` and `propagate ∘ complete` are
monotone and reductive on the abstract state, so lines 4–8 compute the greatest
fixpoint below `⊤` of a decreasing chain; since `𝕃` is finite and fixed at line
2, the chain is finite and the loop terminates.

`𝕃` collects *labels and singleton strings* — a string appearing as a value in
the argument (`{o1: "n1"}`) may become a label in the result (`{n1: 'a}`).

### The abstract state

The state `σ` maps every group `g` (a free label variable being a group with a
single instance and a single column) to

* `U_g`, a finite set of **candidate rows** over the label columns `Λ(g)`, or
  `⊤` when nothing is known yet. A row is a total map `Λ(g) → 𝕃`; a partially
  observed group is represented by keeping `U_g` as a *lazy natural join* of the
  per-occurrence observations (see below) rather than materialising a cartesian
  product;
* `M_g`, a set of **required partial rows**: partial maps `Λ(g) ⇀ 𝕃` such that
  every solution contains at least one instance extending each of them.

Meaning: `M_g ⊑ θ(g) ⊑ U_g` (each element of `M_g` is extended by some instance,
and every instance is a row of `U_g`).

Order: `σ' ⊑ σ` when every `U'_g ⊆ U_g` and every `M'_g ⊇ M_g`. Refinement is
monotone — it can only remove candidates and add requirements — which is the
"monotonicity" the sketch in the previous section asks for.

Type columns are deliberately **not** part of the state: at instantiation time
each type column gets one *fresh type variable per instance*, and the ordinary
type machinery (subtyping/tallying at the application site) solves them. This is
exactly how `'a` is handled by the current implementation of `get`. Types are
only used as a *filter* on candidate rows (see `Compat` below).

### The constraint language

`generate` emits constraints attached to a *record occurrence* — a list, a
parameter list or an attribute list of the scheme, together with the concrete
type it was matched against. For an occurrence `o`, write `E(o)` for its
**entities**: the constant labels, the free label variables in key position, and
the pairs `(g, #c_g)` for each key column of each repetition of `o`.

From the concrete type at `o` we extract

* `Must(o)` — labels certainly present (required fields), an *under*-approximation;
* `May(o)` — labels possibly present, an *over*-approximation, or `⊤` when the
  concrete record is open or not analysable;
* `fieldty(o, ℓ)` — the type of field `ℓ`.

Negative atoms are ignored (they can only shrink `May` and grow `Must`, so
ignoring them keeps both approximations on the safe side). A union of atoms is
handled by generating the constraints of each atom and **joining** the resulting
states, exactly as the current implementation does at the `Arg` level.

The constraints are:

| name | statement |
|---|---|
| `Dom(g, #c, D)` | the image of column `#c` of `g` at this occurrence is included in `D ⊆ 𝕃` |
| `Compat(g, #c, ℓ, T)` | a row with `#c = ℓ` is possible only if `fieldty(o, ℓ)` is compatible with the body type `T` |
| `Obs(g, R)` | the rows of `g` observed at this occurrence form the relation `R`; `U_g ← U_g ⋈ R` |
| `AtMostOne(o, ℓ)` | at most one entity of `o` claims the label `ℓ` (disjointness + injectivity) |
| `AtLeastOne(o, ℓ)` | some entity of `o` claims `ℓ` (coverage) |

`AtMostOne` is emitted for every occurrence and every `ℓ ∈ 𝕃`: it is pure
well-formedness and needs no concrete type. This is what lets the *result* of
`merge` constrain the argument.

`AtLeastOne` and the bounds on `Dom` follow the polarity of the occurrence.
Writing `P` for the scheme's record and `Q` for the concrete one, the domain of
the scheme is compared as `τ ≤ dom` (concrete on the left) and the result as
`ret ≤ ρ` (scheme on the left):

| occurrence | direction | consequences |
|---|---|---|
| domain, `P` closed | `Q ≤ P` | every `ℓ ∈ May(Q)` not taken by a constant must be claimed: `AtLeastOne` |
| domain, body binding required | `Q ≤ P` | claimed labels are present in `Q`: `Dom ⊆ Must(Q)` |
| domain, `P` open | `Q ≤ P` | no coverage constraint; `Dom ⊆ May(Q)` only if bindings are required |
| result, `ρ` closed | `P ≤ Q` | `Dom ⊆ May(ρ)` |
| result, `ρ` required field `ℓ` | `P ≤ Q` | `AtLeastOne(ℓ)` |
| result, `ρ = any` | — | only `AtMostOne` |

Nested contravariant positions (a function type inside a field) flip the
direction; when in doubt, emitting nothing is always sound.

### Constraint generation (the traversal)

`generate` is the existing `match_ty` / `match_field` / `match_param` walk,
extended. Matching a scheme node `pat` against a concrete type `ty`:

* `FRegular _` — nothing.
* `FLVar #x` in type position — `Dom(x, ·, strings_of_ty(pos, ty))`. When `#x`
  is a column of the enclosing repetition, the constraint is *conditioned on the
  current candidate row* (see the repetition case).
* `FAttr` — recurse on `content` and on `attrs`, in `Struct` position.
* `FList {bindings; tl}` / `Arg.atom` — compute `Must`, `May`, `fieldty`, then
  for each binding:
  * **constant key `ℓ`** — recurse on `(T, fieldty(ℓ))`;
  * **free label variable key `#x`** — `Dom(x, ·, May)` and
    `Compat(x, ·, ℓ, T)` for each `ℓ ∈ May`; and, if `σ` already pins
    `#x` to a single `ℓ`, recurse on `(T, fieldty(ℓ))`. *This is the step that
    makes the outer loop necessary*;
  * **repetition `(#c₁: T₁, …, #cₛ: Tₛ)_g`** — for each column `#cₜ`:
    1. candidate domain `Dₜ = { ℓ ∈ May | fieldty(ℓ) compatible with Tₜ }`,
       emitted as `Dom(g, #cₜ, Dₜ)`;
    2. for each `ℓ ∈ Dₜ`, recurse on `(Tₜ, fieldty(ℓ))` in a mode that collects
       the constraints on the *other columns of `g`* conditioned on
       `#cₜ = ℓ`; the resulting set of (partial) rows is the observed relation
       `R`, emitted as `Obs(g, R)`. This is how `rename`'s `renaming` argument
       produces the pairs `(o1,n1)`, `(o2,n2)` without any cartesian product;
    3. constraints produced inside the body that do **not** concern `g`
       (e.g. a free label variable used inside a repetition) must be *joined*
       over the candidate rows, not intersected — we do not know yet which rows
       are selected.
  * finally emit `AtMostOne(o, ℓ)` for all `ℓ`, and `AtLeastOne(o, ℓ)` as
    dictated by the polarity table.

**Compatibility.** `fieldty(ℓ)` is compatible with a body type `T` when
`fieldty(ℓ) ≤ T↑`, where `T↑` is `T` with every instantiable variable replaced
by `any` in covariant position and by `empty` in contravariant position, and
every not-yet-resolved label variable replaced by `chr1` (any singleton string).
`T↑` over-approximates every instance of `T`, so the test only removes rows that
no instantiation could justify. When the check cannot be performed, treat the row
as compatible.

Compatibility is what disambiguates
`(x: {(#r_i: int)_i, (#r_j: chr)_j})` applied to `{a: int, b: chr}`: without it
the exact cover has two solutions and nothing is resolved.

### Propagation (the inner fixpoint)

Introduce one Boolean unknown per group and candidate row:

```
sel[g, ρ]   ≡   "ρ ∈ θ(g)"                        for ρ ∈ U_g
claim[e, ℓ] ≡   ⋁ { sel[g, ρ] | ρ(#c) = ℓ }       for an entity e = (g, #c)
```

and translate:

* `Dom(g, #c, D)` → `¬sel[g, ρ]` for every `ρ` with `ρ(#c) ∉ D`;
* `Compat` → same, per row;
* `Obs(g, R)` → `U_g ← U_g ⋈ R` (rows removed, not a Boolean constraint);
* `AtMostOne(o, ℓ)` → at-most-one over `{ claim[e, ℓ] | e ∈ E(o) }`; note that
  this single family subsumes injectivity (two rows of the same group claiming
  `ℓ` through the same column) and disjointness (two different entities);
* `AtLeastOne(o, ℓ)` → the clause `⋁ { claim[e, ℓ] | e ∈ E(o) }`;
* a constant label `ℓ` of `o` → `claim[ℓ, ℓ]` is true.

`propagate` is unit propagation plus the standard at-most-one / exactly-one
reasoning, run to a fixpoint:

* a forced `sel[g, ρ] = true` adds `ρ` to `M_g` and, by `AtMostOne`, removes
  every conflicting row from the other entities of the same occurrence;
* a forced `sel[g, ρ] = false` removes `ρ` from `U_g`;
* an empty clause, or an entity whose domain becomes empty while a coverage
  clause requires it, means `Θ̂ = ∅` → failure.

This is cheap and handles `test`, `rename`, `get`, and every scheme with at most
one "ambiguous" label per record.

### Completion (the backbone)

Unit propagation is not complete. `merge` is the smallest counter-example: on
the label `s` we end up with the clauses

```
claim[i,s] ∨ claim[j,s]        (coverage of x)
claim[i,s] ∨ claim[k,s]        (coverage of y)
¬claim[j,s] ∨ ¬claim[k,s]      (disjointness in the result)
```

which have no unit, yet `claim[i,s]` holds in every model. The missing step is
**backbone extraction**: a literal is committed iff it is true in every solution
of the residual problem. Two ways to get it, in increasing cost:

1. **failed-literal probing** — for each undecided literal `l`, assume `¬l`, run
   `propagate`; if it fails, commit `l`. Quadratic in the number of literals
   times the cost of propagation, and enough for `merge` and for anything of
   comparable size;
2. **model enumeration** — enumerate the solutions of the residual finite CSP
   (with a budget) and intersect them. Exact within the budget.

Both are sound in the sense of the specification: they only commit literals
entailed by `C`, and `C` over-approximates the real constraints. When the budget
is exhausted, fall back to whatever `propagate` established — the group stays
partially unresolved, which is the intended degraded behaviour.

**The general problem is NP-hard**: the coverage plus disjointness constraints
of a single record are literally an *exact cover* instance, and multi-column
groups add a bipartite-matching flavour on top. So a complete polynomial
algorithm is out of reach, and the layering above (cheap propagation, optional
bounded completion) is the pragmatic answer rather than a shortcut.

### Instantiation

A group `g` is **determined** when `U_g` is finite, every row of `U_g` is ground
on all of `Λ(g)`, and `sel[g, ρ]` is decided for every `ρ ∈ U_g`. Then
`θ(g) = { ρ | sel[g, ρ] }`.

For each determined `g`:

* order `θ(g)` deterministically — by the order in which the key labels appear
  in the concrete argument, then lexicographically — so that the produced
  signature does not depend on internal hash order;
* allocate, **once for the whole scheme**, a fresh type variable `'a^ι` for every
  type column `'a_g` and every instance `ι`. Sharing these across occurrences is
  essential: it is what makes `merge`'s result field `s` have the same type as
  `x`'s and `y`'s, and `rename`'s result field `n1` the type of `x`'s `o1`;
* replace every repetition over `g` by the concatenation of its body over
  `θ(g)`, substituting `θ(#c_g)(ι)` for label columns (as a label in key
  position, as a singleton string type in type position) and `'a^ι` for type
  columns.

Groups and label variables that are not determined are left untouched, so
`specialize` can be applied again with more information (the existing
`partially specialized signatures` test relies on this), and `to_regular` is the
operation that finally reports "unresolved".

### Worked examples

#### `test` — iteration

```
test: (#l: #v, field: #l, ...) -> { #v: #l }
@(l: "v", field: "l")

iteration 1
  toplevel:                 (open parameter list: no coverage constraint)
  field:  #l ∈ {l}          (Dom, from the singleton string "l")
  #l:     key unknown       (skipped)
iteration 2
  #l is pinned to "l" → the parameter named "l" can be selected
  l:      #v ∈ {v}
iteration 3
  stable

result: (l: "v", field: "l", ...: any) -> {v: "l"}
```

This matches the trace in the original sketch:

```
toplevel:

field:
- #l: l

l:
- #v: v
```

#### `rename` — a two-column group

```
rename: (x: { (#o_i : 'a_i)_i }, renaming: { (#o_i : #n_i)_i }) -> { (#n_i : 'a_i) }
@(x: 'b, renaming: { o1: "n1", o2: "n2" })

x:          concrete type is a type variable → May = ⊤, Must = ∅ → no constraint
renaming:   closed vs closed, Must = May = {o1, o2}
              Dom(i, #o, {o1, o2})
              Obs(i, { (o1,n1), (o2,n2) })     -- body type #n_i observed per row
              AtLeastOne(o1), AtLeastOne(o2), AtMostOne(·)
            → exactly-one on o1 and on o2, one candidate each
            → θ(i) = { (o1,n1), (o2,n2) }
result:     AtMostOne on column #n → n1 ≠ n2 ✓ (injectivity of the renaming)

result: (x: {o1: 'a¹, o2: 'a²}, renaming: {o1: "n1", o2: "n2"}) -> {n1: 'a¹, n2: 'a²}
```

which is the original trace, with `'a1`, `'a2` written `'a¹`, `'a²`:

```
renaming:
- i:
    - #o: o1, #n: n1
    - #o: o2, #n: n2

x:
- i:
    - #o: o1, #n: n1, 'a:'a1
    - #o: o2, #n: n2, 'a:'a2
```

#### `merge` — coverage, disjointness and completion

```
merge: (x: { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j }, y: { (#r_i : 'a_i)_i, (#r_k : 'a_k)_k }) -> { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j, (#r_k : 'a_k)_k }
@(x: { x: 1, s: 2 }, y: { y: 3, s: 4 })

generation
  x:      Dom(i,#r,{x,s})  Dom(j,#r,{x,s})   AtLeastOne(x) AtLeastOne(s)  AtMostOne(·)
  y:      Dom(i,#r,{y,s})  Dom(k,#r,{y,s})   AtLeastOne(y) AtLeastOne(s)  AtMostOne(·)
  result:                                                                 AtMostOne(·)

propagation
  Dom(i,#r,{x,s}) ⊓ Dom(i,#r,{y,s})  →  U_i = {s}
  AtLeastOne(x) in x, only j can claim x  →  M_j ∋ x, and (AtMostOne) x ∉ U_i, U_k
  AtLeastOne(y) in y, only k can claim y  →  M_k ∋ y, and             y ∉ U_i, U_j
  s: no unit

completion (probe ¬claim[i,s])
  → claim[j,s] (coverage of x) and claim[k,s] (coverage of y)
  → contradicts AtMostOne(result, s)
  → commit claim[i,s], hence ¬claim[j,s] and ¬claim[k,s]

θ(i) = {s}   θ(j) = {x}   θ(k) = {y}

result: (x: {s: 'aⁱˢ, x: 'aʲˣ}, y: {s: 'aⁱˢ, y: 'aᵏʸ}) -> {s: 'aⁱˢ, x: 'aʲˣ, y: 'aᵏʸ}
```

which is the original trace:

```
x:
- i:
    - #r: x
    - #r: s
- j:
    - #r: x
    - #r: s

y:
- i:
    - #r: s
- j:
    - #r: x
    - #r: s
- k:
    - #r: y
    - #r: s

result:
- i:
    - #r: s
- j:
    - #r: x
- k:
    - #r: y
```

Note that the result occurrence is *needed*, and that it contributes only
well-formedness (`AtMostOne`) constraints — no expected result type is required.
This is the reason for making `specialize` take an optional expected result type
`ρ` (defaulting to `any`): the traversal of the result must happen in any case,
and a caller that knows `ρ` gets extra constraints for free.

### Termination, complexity, safeguards

* **Termination** — `𝕃` is fixed before the loop; `U_g ⊆ 𝕃^{Λ(g)}` only shrinks
  and `M_g` only grows, so the abstract state has finite height and the outer
  loop stops. In practice it converges in 2–3 iterations (one per level of
  "label used as a key to reach another label").
* **Row blow-up** — the danger is a group whose columns are never observed
  together: materialising `U_g` would cost `|𝕃|^{|Λ(g)|}`. Keeping `U_g` as a
  lazy join of per-occurrence observations avoids it in every realistic scheme
  (a repetition observes all its columns at once). Cap the materialised size and
  fall back to `U_g = ⊤` (unresolved) rather than failing.
* **Completion budget** — cap probing/enumeration; falling back to the result of
  `propagate` is always sound.
* **Cost of the common case** — a scheme with no group and no repeated label is
  handled by `propagate` alone, in one or two traversals; the added machinery
  costs nothing there.

### Representation notes

A minimal change to `FunSig` that covers all the examples: restrict a repetition
body to a *single* binding. Then a repetition is still an `(label, ty)` pair and
fits the existing `Lst.atom` / `Arg.atom` shapes:

```ocaml
type label =
| LConst of string
| LVar of string
| LGroupVar of { group: string ; column: string }   (* new *)

type ('v,'r,'i) ty =
| FLVar of string
| FGroupVar of { group: string ; column: string }   (* new: #n_i in type position *)
| ...
```

plus, on the signature itself, the list of groups with, per group, its label
columns and its type columns (the latter being ordinary `Var.t`s flagged as
indexed, so that `instantiate` knows to freshen them per instance):

```ocaml
type ('v,'r,'i) t = {
  dom: ('v,'r,'i) arg ;
  ret: ('v,'r,'i) ty ;
  groups: group_info StrMap.t ;                      (* new *)
}
```

Multi-binding bodies (`(#a_i: 'x_i, #b_i: 'y_i)_i`) would require a dedicated
binding constructor and are left for later; the algorithm above is written for
the general case, so only the representation would change.

### Alternatives considered

* **Keep the current one-pass, purely local matching.** Cannot resolve `test`
  (a label variable used as a key), and has no place to express coverage or
  disjointness, so groups are out of reach.
* **Encode groups as row polymorphism and let tallying solve them.** The
  cardinality of a group is not expressible in the type algebra (a record type
  has a fixed set of labels), and even if the constraint were solvable we would
  still need the labels *syntactically* to build the specialized signature.
  Tallying stays the right tool for the *type* columns, which is exactly the
  division of labour proposed above.
* **A dedicated partition algorithm on label sets.** Simpler, and enough for
  `merge`, but it cannot link two columns of the same group, so `rename` is out.
  The relational formulation degenerates to it when every group has one column.
* **Formal relative.** The problem is a form of *set/AC unification with
  sequence variables* over a finite ground signature. The finite-domain CSP
  encoding is a standard way to attack it, and it is what makes the "commit the
  backbone" rule a clean generalisation of today's "commit singletons" rule.

### Open questions

1. **Nested repetitions** (`((#a_ij: 't_ij)_j)_i`) — the inner index set would be
   per outer instance, i.e. `θ(j)` becomes a function of `ι ∈ θ(i)`. The
   abstract state generalises (rows over `Λ(i) ⊎ Λ(j)`), but the coverage
   constraints get harder. Forbidden in v1.
2. **Ordering** — should the expanded fields keep the order of the concrete
   argument? It matters for `Arg` (positional-or-named parameters) more than for
   `Lst`.
3. **Repetitions over positional parameters** — indexed by position rather than
   by label; needs a different identity for instances (a natural number), and a
   different set of constraints (contiguity instead of disjointness).
4. **Optional bindings inside a repetition** (`(#r_i: 'a_i?)_i`) — the coverage
   direction changes; probably worth forbidding.
5. **How much completion is worth doing by default?** Failed-literal probing is
   probably the right default (it is what `merge` needs and it is cheap on
   schemes of realistic size), with model enumeration behind a flag.
