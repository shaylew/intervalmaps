# IntervalExchange

This is a Haskell version of https://github.com/faec/IntervalExchange
(originally in Swift). If you're looking at this, you're probably in the study
group with me and know where to direct any questions. If you're not, the survey
paper we're working from is
http://www.mat.ucm.es/serv/revmat/vol19-1/vol19-1a_p.pdf, and may God have mercy
on your soul.

## To build

```
stack build
```

## To test

```
stack test
```

(If you end up playing with this you may also be interested in the
`--file-watch` variants of the above two commands.)

## To run interactively

```
stack repl
```

Certain modules reuse common names and are designed to be imported qualified, so
you may want to do something like this at the start of your ghci session:

```
:module Preliminaries
import qualified Permutation as P
```

## To generate documentation

We're working on it. For now, here's a rough outline. Some of these names might
not be the best; we're open to suggestions.

### Preliminaries

This contains some general and simple utilities, and a couple of classes that
many of the mathematical objects we're interested will be instances of. The
`Maplike` and `InverseMaplike` classes aren't very Haskell-y but they're
convenient when we have so many objects (permutations; interval partitions;
interval translation maps, ...) that can be seen as functions with some extra
structure.

### Permutation

What it says on the can. Under the hood a permutation `p` is represented as a
dense array whose *i*<sup>th</sup> element is `p(i)`. One slightly weird choice
is that permutations are defined everywhere -- an index that's out of range gets
mapped to itself. This lets us get away with only having one `identity`.

### Enumeration

An `Enumeration a` is morally a bijection from a finite set `a` to the set `{1,
..., n}`. There's nothing interesting in this module and it's possible it'll go
away eventually, but for now we're using it.

### IntervalMap

Morally an `IntervalMap k a` is (1) a collection of adjacent intervals (whose
endpoints are of type `k`) with (2) a value of type `a` assigned to each one.
You can think of this as a piecewise constant function from `k` to `a`. The
fancy operation is `zip`, which combines two such functions pointwise.

```zip :: IntervalMap k a -> IntervalMap k b -> IntervalMap k (a, b)```
```zip f g `at` x == (f `at` x, g `at` x)```

### IntervalExchange

Defines `IEM k a`, an interval ~~exchange~~ translation map over a field `k`
with interval labels in `a`. This is a little bit ugly right now but it should
be followable. The real beast is `compose` and its helper functions `chunkBy`
and `expandBy`.
