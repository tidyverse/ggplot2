# facet_grid() handles rows/cols correctly

    `rows` must be `NULL` or a `vars()` list if `cols` is a `vars()` list.

---

    `cols` must be a `vars()` specification or `NULL`, not a <formula> object.

# facet_grid() throws errors at bad layout specs

    `facet_grid()` can't use free scales with `coord_cartesian()` with a fixed `ratio` argument.

---

    Free scales cannot be mixed with a fixed aspect ratio.

# facet_grid() warns about bad switch input

    `switch` must be one of "both", "x", or "y", not "z".

