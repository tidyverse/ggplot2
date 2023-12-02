# scale_apply preserves class and attributes

    Code
      scale_apply(df, "x", "transform", c(NA, 1), plot$layout$panel_scales_x)
    Condition
      Error in `scale_apply()`:
      ! `scale_id` must not contain any "NA".

# breaks and labels are correctly checked

    Code
      check_breaks_labels(1:10, letters)
    Condition
      Error:
      ! `breaks` and `labels` must have the same length.

---

    Code
      ggplot_build(p)
    Condition
      Error in `scale_x_continuous()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      ggplot_build(p)
    Condition
      Error in `scale_x_continuous()`:
      ! Invalid `minor_breaks` specification. Use `NULL`, not `NA`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `scale_x_continuous()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `scale_x_continuous()`:
      ! `breaks` and `labels` have different lengths.

---

    Code
      ggplot_build(p)
    Condition
      Error in `scale_x_discrete()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `scale_x_discrete()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      ggplot_build(p)
    Condition
      Error in `scale_x_binned()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `scale_x_binned()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `scale_x_binned()`:
      ! `breaks` and `labels` have different lengths.

# numeric scale transforms can produce breaks

    Code
      test_breaks("log", limits = c(0.1, 1000))
    Output
      [1]        NA   1.00000  20.08554 403.42879

# training incorrectly appropriately communicates the offenders

    Code
      sc$train(1:5)
    Condition
      Error in `scale_colour_viridis_d()`:
      ! Continuous values supplied to discrete scale.
      i Example values: 1, 2, 3, 4, and 5

---

    Code
      sc$train(LETTERS[1:5])
    Condition
      Error in `scale_colour_viridis_c()`:
      ! Discrete values supplied to continuous scale.
      i Example values: "A", "B", "C", "D", and "E"

# Using `scale_name` prompts deprecation message

    Code
      res <- continuous_scale("x", "foobar", identity_pal())
    Condition
      Warning:
      The `scale_name` argument of `continuous_scale()` is deprecated as of ggplot2 3.5.0.
    Code
      res <- discrete_scale("x", "foobar", identity_pal())
    Condition
      Warning:
      The `scale_name` argument of `discrete_scale()` is deprecated as of ggplot2 3.5.0.
    Code
      res <- binned_scale("x", "foobar", identity_pal())
    Condition
      Warning:
      The `scale_name` argument of `binned_scale()` is deprecated as of ggplot2 3.5.0.

