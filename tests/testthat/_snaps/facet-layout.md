# facet_wrap throws errors at bad layout specs

    `ncol` must be a whole number or `NULL`, not an integer vector.

---

    `ncol` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    `ncol` must be a whole number or `NULL`, not the number 1.5.

---

    `nrow` must be a whole number or `NULL`, not an integer vector.

---

    `nrow` must be a whole number larger than or equal to 1 or `NULL`, not the number -1.

---

    `nrow` must be a whole number or `NULL`, not the number 1.5.

---

    Need 3 panels, but together `nrow` and `ncol` only provide 1.
    i Please increase `ncol` and/or `nrow`.

---

    `facet_wrap()` can't use free scales with `coord_fixed()`.

# facet_grid throws errors at bad layout specs

    `coord_fixed()` doesn't support free scales.

---

    Free scales cannot be mixed with a fixed aspect ratio.

# facet_wrap and facet_grid throws errors when using reserved words

    "ROW" is not an allowed name for faceting variables.
    i Change the name of your data columns to not be "PANEL", "ROW", "COL", "SCALE_X", or "SCALE_Y".

---

    "ROW" and "PANEL" are not allowed names for faceting variables.
    i Change the name of your data columns to not be "PANEL", "ROW", "COL", "SCALE_X", or "SCALE_Y".

---

    "ROW" is not an allowed name for faceting variables.
    i Change the name of your data columns to not be "PANEL", "ROW", "COL", "SCALE_X", or "SCALE_Y".

