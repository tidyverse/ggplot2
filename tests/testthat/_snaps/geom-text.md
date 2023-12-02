# geom_text() checks input

    Code
      geom_text(position = "jitter", nudge_x = 0.5)
    Condition
      Error in `geom_text()`:
      ! Both `position` and `nudge_x`/`nudge_y` are supplied.
      i Only use one approach to alter the position.

