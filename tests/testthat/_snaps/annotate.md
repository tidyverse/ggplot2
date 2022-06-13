# annotation_raster() and annotation_custom() requires cartesian coordinates

    Problem while converting geom to grob.
    i Error occurred in the 1st layer.
    Caused by error in `draw_panel()`:
    ! `annotation_raster()` only works with `coord_cartesian()`

---

    Problem while converting geom to grob.
    i Error occurred in the 1st layer.
    Caused by error in `draw_panel()`:
    ! `annotation_custom()` only works with `coord_cartesian()`

# annotation_map() checks the input data

    `map` must be a <data.frame>

---

    `map` must have the columns `x`, `y`, and `id`

# unsupported geoms signal a warning (#4719)

    `geom` must not be "hline".
    i Please use `geom_hline()` directly instead.

# annotate() checks aesthetic lengths match

    Unequal parameter lengths: x (3), y (3), and fill (2)

