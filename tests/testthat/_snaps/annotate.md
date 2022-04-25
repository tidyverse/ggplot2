# annotation_raster() and annotation_custom() requires cartesian coordinates

    `annotation_raster()` only works with `coord_cartesian()`

---

    `annotation_custom()` only works with `coord_cartesian()`

# unsupported geoms signal a warning (#4719)

    `annotate()` does not support `geom = "hline"`.
    i Please use `geom_hline()` directly instead.
