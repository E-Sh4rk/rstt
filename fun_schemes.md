# Function schemes

## Syntax

TODO

## Label variables

## Groups

```
merge: (x: { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j }, y: { (#r_i : 'a_i)_i, (#r_k : 'a_k)_k }) -> { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j, (#r_k : 'a_k)_k }
rename: (x: { (#o_i : 'a_i)_i }, renaming: { (#o_i : #n_i)_i }) -> { (#n_i : 'a_i) }
test: (#l: #v, field: #l, ...) -> { #v: #l }
```

## Specialization algorithm

Resolution is iterative, each iteration walks through the concrete argument type and scheme, and refines an environment which constrains which values a label variable or group could take. Refinement is monotonic in the sense that it can only remove possibilities.

It may be necessary to go through the result, not only the argument, to gather disjointness constraints for some groups ; the concrete type of the result is most likely not known, but still the specialization function could optionaly take it as argument (in addition to the concrete type of the argument) and give it the type 'any' by default.

Example: specialization process for `test` with argument `@(l: "v", field: "l")`
```
test: (#l: #v, field: #l, ...) -> { #v: #l }
@(l: "v", field: "l")

toplevel:

field:
- #l: l

l:
- #v: v
```

Example: specialization process for `rename` with argument `@(x: 'b, renaming: { o1: "n1", o2: "n2" })`
```
rename: (x: { (#o_i : 'a_i)_i }, renaming: { (#o_i : #n_i)_i }) -> { (#n_i : 'a_i) }
@(x: 'b, renaming: { o1: "n1", o2: "n2" })

toplevel:

renaming:
- i:
    - #o: o1, #n: n1
    - #o: o2, #n: n2

x:
- i:
    - #o: o1, #n: n1, 'a:'a1
    - #o: o2, #n: n2, 'a:'a2
```

Example: specialization process for `merge` with argument `@(x: { x: 1, s: 2 }, y: { y: 3, s: 4 })`
```
merge: (x: { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j }, y: { (#r_i : 'a_i)_i, (#r_k : 'a_k)_k }) -> { (#r_i : 'a_i)_i, (#r_j : 'a_j)_j, (#r_k : 'a_k)_k }
@(x: { x: 1, s: 2 }, y: { y: 3, s: 4 })

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
