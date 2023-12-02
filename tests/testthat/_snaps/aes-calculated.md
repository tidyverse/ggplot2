# staged aesthetics warn appropriately for duplicated names

    Code
      p <- ggplot(df, aes(x, y, label = lab)) + geom_label(aes(colour = stage(lab,
        after_scale = colour), color = after_scale(color))) + guides(colour = "none")
    Condition
      Warning:
      Duplicated aesthetics after name standardisation: colour

# A deprecated warning is issued when stat(var) or ..var.. is used

    Code
      out <- ggplot_build(p1)
    Condition
      Warning:
      `stat(foo)` was deprecated in ggplot2 3.4.0.
      i Please use `after_stat(foo)` instead.

---

    Code
      out <- ggplot_build(p2)
    Condition
      Warning:
      The dot-dot notation (`..bar..`) was deprecated in ggplot2 3.4.0.
      i Please use `after_stat(bar)` instead.

