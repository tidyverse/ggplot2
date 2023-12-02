# binwidth is respected

    Code
      res <- ggplot_build(p)
    Condition
      Warning:
      Computation failed in `stat_bin2d()`.
      Caused by error in `bin2d_breaks()`:
      ! `binwidth` must be a number, not a double vector.

---

    Code
      res <- ggplot_build(p)
    Condition
      Warning:
      Computation failed in `stat_bin2d()`.
      Caused by error in `bin2d_breaks()`:
      ! `origin` must be a number, not a double vector.

