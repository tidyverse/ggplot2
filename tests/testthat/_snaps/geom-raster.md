# geom_raster() checks input and coordinate system

    `hjust` must be a number, not a double vector.

---

    `hjust` must be a number, not the string "a".

---

    `vjust` must be a number, not a double vector.

---

    `vjust` must be a number, not the string "a".

---

    Code
      b <- ggplotGrob(p)
    Message
      `geom_raster()` only works with linear coordinate systems, not `coord_polar()`.
      i Falling back to drawing as `geom_rect()`.

# geom_raster() fails with pattern fills

    Problem while converting geom to grob.
    i Error occurred in the 1st layer.
    Caused by error in `draw_panel()`:
    ! `geom_raster()` cannot render pattern fills.

