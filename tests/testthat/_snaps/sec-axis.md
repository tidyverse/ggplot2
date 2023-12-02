# sec_axis checks the user input

    Code
      set_sec_axis(16, scale)
    Condition
      Error in `set_sec_axis()`:
      ! Secondary axes must be specified using `sec_axis()`.

---

    Code
      secondary$init(scale)
    Condition
      Error in `init()`:
      ! Transformation for secondary axes must be a function.

---

    Code
      ggplot_build(p)
    Condition
      Error in `mono_test()`:
      ! Transformation for secondary axes must be monotonic.

