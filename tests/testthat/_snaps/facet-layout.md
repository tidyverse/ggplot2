# facet_wrap throws errors at bad layout specs

    Code
      facet_wrap(~test, ncol = 1:4)
    Condition
      Error in `facet_wrap()`:
      ! `ncol` must be a whole number or `NULL`, not an integer vector.

---

    Code
      facet_wrap(~test, ncol = -1)
    Condition
      Error in `facet_wrap()`:
      ! `ncol` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    Code
      facet_wrap(~test, ncol = 1.5)
    Condition
      Error in `facet_wrap()`:
      ! `ncol` must be a whole number or `NULL`, not the number 1.5.

---

    Code
      facet_wrap(~test, nrow = 1:4)
    Condition
      Error in `facet_wrap()`:
      ! `nrow` must be a whole number or `NULL`, not an integer vector.

---

    Code
      facet_wrap(~test, nrow = -1)
    Condition
      Error in `facet_wrap()`:
      ! `nrow` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    Code
      facet_wrap(~test, nrow = 1.5)
    Condition
      Error in `facet_wrap()`:
      ! `nrow` must be a whole number or `NULL`, not the number 1.5.

---

    Code
      ggplot_build(p)
    Condition
      Error in `wrap_dims()`:
      ! Need 3 panels, but together `nrow` and `ncol` only provide 1.
      i Please increase `ncol` and/or `nrow`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `draw_panels()`:
      ! `facet_wrap()` can't use free scales with `coord_fixed()`.

# facet_grid throws errors at bad layout specs

    Code
      ggplotGrob(p)
    Condition
      Error in `draw_panels()`:
      ! `coord_fixed()` doesn't support free scales.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `draw_panels()`:
      ! Free scales cannot be mixed with a fixed aspect ratio.

# facet_wrap and facet_grid throws errors when using reserved words

    Code
      ggplotGrob(p + facet_grid(ROW ~ gear))
    Condition
      Error in `facet_grid()`:
      ! "ROW" is not an allowed name for faceting variables.
      i Change the name of your data columns to not be "PANEL", "ROW", "COL", "SCALE_X", or "SCALE_Y".

---

    Code
      ggplotGrob(p + facet_grid(ROW ~ PANEL))
    Condition
      Error in `facet_grid()`:
      ! "ROW" and "PANEL" are not allowed names for faceting variables.
      i Change the name of your data columns to not be "PANEL", "ROW", "COL", "SCALE_X", or "SCALE_Y".

---

    Code
      ggplotGrob(p + facet_wrap(~ROW))
    Condition
      Error in `facet_wrap()`:
      ! "ROW" is not an allowed name for faceting variables.
      i Change the name of your data columns to not be "PANEL", "ROW", "COL", "SCALE_X", or "SCALE_Y".

