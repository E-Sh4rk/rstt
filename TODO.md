# TODO

## New args

```
Pos:1,2,3,4	Named:x,y,z	NamedTl:`r
@(1,2,3,4 ; x="x",y="y",z="z",`r)
{ _npos:4, _pos: {_1:1,_2:2,_3:3,_4:4}, x:"x", y:"y", z:"z" ; `r }

PosNamed:a,b,c,d	Named:x,y,z	PosTl:'a	NamedTl:`r
(a=1,b=2,c=3,d=4 ; 'a,`r ; x="x",y="y",z="z")
{ _npos:4.., _pos: {_1:1,_2:2,_3:3,_4:4 ; 'a}, x:"x", y:"y", z:"z" ; `r }
{ _npos:3, _pos: {_1:1,_2:2,_3:3}, d:4, x:"x", y:"y", z:"z" ; `r }
...
```

## New lists

```
Bindings:x,y,z  Tl:`r
{ x="x",y="y",z="z",`r }
{ _npos: empty?, _pos: empty?, x:"x", y:"y", z:"z" ; `r }
```
