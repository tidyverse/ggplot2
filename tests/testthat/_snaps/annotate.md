# annotation_raster() and annotation_custom() requires cartesian coordinates

    Code
      ggplotGrob(p)
    Condition
      Error in `annotation_raster()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_panel()`:
      ! `annotation_raster()` only works with `coord_cartesian()`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `annotation_custom()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `draw_panel()`:
      ! `annotation_custom()` only works with `coord_cartesian()`.

# annotation_map() checks the input data

    Code
      annotation_map(letters)
    Condition
      Error in `annotation_map()`:
      ! `map` must be a data frame, not a character vector.
    Code
      annotation_map(mtcars)
    Condition
      Error in `annotation_map()`:
      ! `map` must have the columns `x`, `y`, and `id`.

# unsupported geoms signal a warning (#4719)

    Code
      out <- annotate("hline", yintercept = 0)
    Condition
      Warning:
      `geom` must not be "hline".
      i Please use `geom_hline()` directly instead.

# annotate() checks aesthetic lengths match

    Code
      annotate("point", 1:3, 1:3, fill = c("red", "black"))
    Condition
      Error in `annotate()`:
      ! Unequal parameter lengths: x (3), y (3), and fill (2)

# annotation_logticks warns about deprecated `size` argument

    Code
      out <- annotation_logticks(size = 5)
    Condition
      Warning:
      Using the `size` aesthetic in this geom was deprecated in ggplot2 3.5.0.
      i Please use `linewidth` instead.

