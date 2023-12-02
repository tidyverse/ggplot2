# geom_map() checks its input

    Code
      geom_map(map = letters)
    Condition
      Error in `geom_map()`:
      ! `map` must be a data frame, not a character vector.

---

    Code
      geom_map(map = mtcars)
    Condition
      Error in `geom_map()`:
      ! `map` must have the columns `x`, `y`, and `id`.

