# v0.2.1.0

- Remove most of `Ord a` constraints, which are unnecessary since we already have a `a -> a -> Ordering` as input.

# v0.2.0.0

- `Data.SortingNetwork` now exports sorting functions based on optimal network (by size).
- Added proper type signatures to generated functions (thanks to u/brandonchinn178).
- Renamed `sortListXBy` to `unsafeSortListBy`, and few changes to improve runtime safety (thanks to u/Endicy).
- Improved documentation.

# v0.1.0.0

Initial version.
