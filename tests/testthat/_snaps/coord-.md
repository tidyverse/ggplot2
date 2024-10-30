# Coord errors on missing methods

    `coord()` has not implemented a `render_bg()` method.

---

    `coord()` has not implemented a `render_axis_h()` method.

---

    `coord()` has not implemented a `render_axis_v()` method.

---

    `coord()` has not implemented a `backtransform_range()` method.

---

    `coord()` has not implemented a `range()` method.

# check coord limits errors only on bad inputs

    Code
      check_coord_limits(xlim(1, 2))
    Condition
      Error:
      ! `xlim(1, 2)` must be a vector, not a <ScaleContinuousPosition> object.

---

    Code
      check_coord_limits(1:3)
    Condition
      Error:
      ! `1:3` must be a vector of length 2, not length 3.

