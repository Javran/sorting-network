# sorting-network

Sort small lists with sorting network.

## How to use

This library provides functions that can sort small (2 ~ 16 elements),
fixed-sized list / homogenous tuple of elements.

To use this library, `import Data.SortingNetwork` and you will bring two sets of functions into scope:

```haskell
-- where X is a number from 2-16
unsafeSortListXBy :: Ord a => (a -> a -> Ordering) -> [a] -> [a]

-- where X is a number from 2-16
sortTup2By :: Ord a => (a -> a -> Ordering) -> (a, a) -> (a, a)
sortTup3By :: Ord a => (a -> a -> Ordering) -> (a, a, a) -> (a, a, a)
sortTup4By :: Ord a => (a -> a -> Ordering) -> (a, a, a, a) -> (a, a, a, a)
...
sortTup16By :: Ord a => (a -> a -> Ordering) -> (a, a, ..., a) -> (a, a, ..., a)
```

in which `unsafeSortListXBy` are partial functions that only accept list of corresponding length
(you will get pattern matching failures if that is not the case).

## How it works

A [sorting network](https://en.wikipedia.org/wiki/Sorting_network) can sort fixed size list of elements.

Observe that the following function sorts a tuple of 3 elements (name shadowing is intentional for demostration):

```haskell
sortTup3By :: Ord a => (a -> a -> Ordering) -> (a, a, a) -> (a, a, a)
sortTup3By = \cmp (a, b, c) ->
  let sw = \u v f ->
        if cmp u v == GT then f v u else f u v
   in sw a b \a b ->
        sw a c \a c ->
          sw b c \b c ->
            (a, b, c)
```

This works because it simulates a sorting network of length 3:
if we pretend variable `a`, `b`, and `c` are 3 wires in the network,
and `sw a b \a b -> {body}` applies a comparator on wire `a` and `b`, shadowing
old variables with now properly ordered `a` and `b` in the `{body}`.

To generalize, if we have `n` variables and a sorting network known to sort `n` elements,
we can build a sort function of fixed size `n` by building sorting network with
comparators applied layer-by-layer in that order.

This library builds those functions for you, utilizing template haskell.

## Acknowledgment

This library takes inspiration from @oisdk's post [Sorting Small Things in Haskell](https://doisinkidney.com/posts/2018-05-06-sorting-small.html).

Also thanks for all the suggestion, support, and criticism from [the initial release thread on Reddit](https://www.reddit.com/r/haskell/comments/11eyopo/ann_sortingnetwork_initial_release/).
