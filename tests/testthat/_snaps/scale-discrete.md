# Scale is checked in default colour scale

    Code
      scale_colour_discrete(type = scale_colour_gradient)
    Condition
      Error in `scale_colour_discrete()`:
      ! The `type` argument must return a discrete scale for the colour aesthetic.
      x The provided scale is continuous.

---

    Code
      scale_fill_discrete(type = scale_fill_gradient)
    Condition
      Error in `scale_fill_discrete()`:
      ! The `type` argument must return a discrete scale for the fill aesthetic.
      x The provided scale is continuous.

---

    Code
      scale_colour_discrete(type = scale_fill_hue)
    Condition
      Error in `scale_colour_discrete()`:
      ! The `type` argument must return a continuous scale for the colour aesthetic.
      x The provided scale works with the following aesthetics: fill.

---

    Code
      scale_fill_discrete(type = scale_colour_hue)
    Condition
      Error in `scale_fill_discrete()`:
      ! The `type` argument must return a continuous scale for the fill aesthetic.
      x The provided scale works with the following aesthetics: colour.

# Aesthetics with no continuous interpretation fails when called

    A continuous variable cannot be mapped to the linetype aesthetic.
    i Choose a different aesthetic or use `scale_linetype_binned()`.

---

    A continuous variable cannot be mapped to the shape aesthetic.
    i Choose a different aesthetic or use `scale_shape_binned()`.

# mapped_discrete vectors behaves as predicted

    Code
      mapped_discrete(letters)
    Condition
      Error in `mapped_discrete()`:
      ! Can't convert `x` <character> to <double>.

# invalid palettes trigger errors

    Code
      ggplot_build(p + scale_x_discrete(palette = function(x) LETTERS[1:3]))
    Condition
      Error in `scale_x_discrete()`:
      ! The `palette` function must return a <numeric> vector.

---

    Code
      ggplot_build(p + scale_x_discrete(palette = function(x) 1:2))
    Condition
      Error in `scale_x_discrete()`:
      ! The `palette` function must return at least 3 values.

