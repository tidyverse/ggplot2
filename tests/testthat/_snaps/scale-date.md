# date(time) scales throw warnings when input is incorrect

    A <numeric> value was passed to a Date scale.
    i The value was converted to a <Date> object.

---

    A <numeric> value was passed to a Datetime scale.
    i The value was converted to a <POSIXt> object.

---

    Code
      ggplot_build(p + scale_x_date(date_breaks = c(11, 12)))
    Condition
      Error in `datetime_scale()`:
      ! `date_breaks` must be a single string, not a double vector.

---

    Code
      ggplot_build(p + scale_x_date(date_minor_breaks = c(11, 12)))
    Condition
      Error in `datetime_scale()`:
      ! `date_minor_breaks` must be a single string, not a double vector.

---

    Code
      ggplot_build(p + scale_x_date(date_labels = c(11, 12)))
    Condition
      Error in `datetime_scale()`:
      ! `date_labels` must be a single string, not a double vector.

