# labels match breaks

    Code
      scale_x_discrete(breaks = 1:3, labels = 1:2)
    Condition
      Error in `scale_x_discrete()`:
      ! `breaks` and `labels` must have the same length.

---

    Code
      scale_x_continuous(breaks = 1:3, labels = 1:2)
    Condition
      Error in `scale_x_continuous()`:
      ! `breaks` and `labels` must have the same length.

# passing continuous limits to a discrete scale generates a warning

    Continuous limits supplied to discrete scale.
    i Did you mean `limits = factor(...)` or `scale_*_continuous()`?

# suppressing breaks, minor_breask, and labels works

    Code
      scale_x_date(breaks = NA, limits = lims)$get_breaks()
    Condition
      Error in `scale_x_date()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_date(labels = NA, limits = lims)$get_labels()
    Condition
      Error in `scale_x_date()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_date(minor_breaks = NA, limits = lims)$get_breaks_minor()
    Condition
      Error in `scale_x_date()`:
      ! Invalid `minor_breaks` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_datetime(breaks = NA, limits = lims)$get_breaks()
    Condition
      Error in `scale_x_datetime()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_datetime(labels = NA, limits = lims)$get_labels()
    Condition
      Error in `scale_x_datetime()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_datetime(minor_breaks = NA, limits = lims)$get_breaks_minor()
    Condition
      Error in `scale_x_datetime()`:
      ! Invalid `minor_breaks` specification. Use `NULL`, not `NA`.

