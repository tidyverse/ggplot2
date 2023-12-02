# geom_label() throws meaningful errors

    Code
      geom_label(position = "jitter", nudge_x = 0.5)
    Condition
      Error in `geom_label()`:
      ! Both `position` and `nudge_x`/`nudge_y` are supplied.
      i Choose one approach to alter the position.

---

    Code
      labelGrob(label = 1:3)
    Condition
      Error in `labelGrob()`:
      ! `label` must be of length 1.

