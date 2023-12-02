# error is thrown with wrong quantile input

    Code
      p <- ggplot(mtcars, aes(sample = mpg)) + stat_qq(quantiles = 1:5)
      res <- ggplot_build(p)
    Condition
      Warning:
      Computation failed in `stat_qq()`.
      Caused by error in `compute_group()`:
      ! The length of `quantiles` must match the length of the data.
    Code
      p <- ggplot(mtcars, aes(sample = mpg)) + geom_qq_line(quantiles = 1:5)
      res <- ggplot_build(p)
    Condition
      Warning:
      Computation failed in `stat_qq_line()`.
      Caused by error in `compute_group()`:
      ! `quantiles` must have the same length as the data.
    Code
      p <- ggplot(mtcars, aes(sample = mpg)) + geom_qq_line(line.p = 0.15)
      res <- ggplot_build(p)
    Condition
      Warning:
      Computation failed in `stat_qq_line()`.
      Caused by error in `compute_group()`:
      ! Cannot fit line quantiles 0.15. `line.p` must have length 2.

