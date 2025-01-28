# geom_text() checks input

    Ignoring unknown parameters: `nudge_x`

# geom_text() drops missing angles

    Removed 1 row containing missing values or values outside the scale range (`geom_text()`).

# geom_text() rejects exotic units

    Code
      ggplotGrob(p + geom_text(size = 10, size.unit = "npc"))
    Condition
      Error in `geom_text()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 1st layer.
      Caused by error in `resolve_text_unit()`:
      ! `unit` must be one of "mm", "pt", "cm", "in", or "pc", not "npc".
      i Did you mean "pc"?

