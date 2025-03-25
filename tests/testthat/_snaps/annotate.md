# annotation_raster() and annotation_custom() requires cartesian coordinates

    Problem while converting geom to grob.
    i Error occurred in the 1st layer.
    Caused by error in `ranges_annotation()`:
    ! `annotation_raster()` only works with `coord_cartesian()`.

---

    Problem while converting geom to grob.
    i Error occurred in the 1st layer.
    Caused by error in `ranges_annotation()`:
    ! `annotation_custom()` only works with `coord_cartesian()`.

# annotation_map() checks the input data

    `map` must be a data frame, not a character vector.

---

    `map` must have the columns `x`, `y`, and `id`.

# unsupported geoms signal a warning (#4719)

    `geom` must not be "hline".
    i Please use `geom_hline()` directly instead.

# annotate() checks aesthetic lengths match

    Unequal parameter lengths: x (3), y (3), and fill (2)

# annotation_logticks warns about deprecated `size` argument

    Using the `size` aesthetic in this geom was deprecated in ggplot2 3.5.0.
    i Please use `linewidth` instead.

# annotate() warns about `stat` or `position` arguments

    `annotate()` can't accept `stat` or `position` arguments.

