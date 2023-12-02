# geom_raster() checks input and coordinate system

    Code
      geom_raster(hjust = c(2.5, 1.3))
    Condition
      Error in `geom_raster()`:
      ! `hjust` must be a number, not a double vector.

---

    Code
      geom_raster(hjust = "a")
    Condition
      Error in `geom_raster()`:
      ! `hjust` must be a number, not the string "a".

---

    Code
      geom_raster(vjust = c(2.5, 1.3))
    Condition
      Error in `geom_raster()`:
      ! `vjust` must be a number, not a double vector.

---

    Code
      geom_raster(vjust = "a")
    Condition
      Error in `geom_raster()`:
      ! `vjust` must be a number, not the string "a".

---

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_raster()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_panel()`:
      ! `geom_raster()` only works with `coord_cartesian()`.

