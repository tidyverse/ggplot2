# geom_map() checks its input

    `map` must be a data frame, not a character vector.

---

    `map` must have the columns `x`, `y`, and `id`.

# map_data() checks it input

    Code
      map_data("world", namesonly = TRUE)
    Condition
      Error in `map_data()`:
      ! `maps::map()` must return an object of type <map>, not a character vector.
      i Did you pass the right arguments?

