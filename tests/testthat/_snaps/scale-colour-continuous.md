# type argument is checked for proper input

    Code
      scale_colour_continuous(type = function() "abc")
    Condition
      Error in `scale_colour_continuous()`:
      ! The `type` argument must return a continuous scale for the colour aesthetic.
      x The provided object is not a scale function.

---

    Code
      suppressWarnings(scale_fill_continuous(type = geom_point))
    Condition
      Error in `scale_fill_continuous()`:
      ! The `type` argument must return a continuous scale for the fill aesthetic.
      x The provided object is not a scale function.

---

    Code
      scale_colour_binned(type = function(...) scale_colour_binned(aesthetics = c(
        "fill", "point_colour")))
    Condition
      Error in `scale_colour_binned()`:
      ! The `type` argument must return a continuous scale for the colour aesthetic.
      x The provided scale works with the following aesthetics: fill and point_colour.

---

    Code
      scale_fill_binned(type = scale_fill_brewer)
    Condition
      Error in `scale_fill_binned()`:
      ! The `type` argument must return a continuous scale for the fill aesthetic.
      x The provided scale is discrete.

---

    Code
      scale_fill_continuous(type = "abc")
    Condition
      Error in `scale_fill_continuous()`:
      ! Unknown scale type: "abc"
      i Use either "gradient" or "viridis".

---

    Code
      scale_colour_continuous(type = "abc")
    Condition
      Error in `scale_colour_continuous()`:
      ! Unknown scale type: "abc"
      i Use either "gradient" or "viridis".

