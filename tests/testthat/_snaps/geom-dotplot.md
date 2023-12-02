# NA's result in warning from stat_bindot

    Code
      out <- ggplot_build(ggplot(dat, aes(x)) + geom_dotplot(binwidth = 0.2))
    Condition
      Warning:
      Removed 2 rows containing missing values or values outside the scale range (`stat_bindot()`).

# weight aesthetic is checked

    Code
      p <- ggplot(mtcars, aes(x = mpg, weight = gear / 3)) + geom_dotplot()
      e <- ggplot_build(p)
    Message
      Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.
    Condition
      Warning:
      Computation failed in `stat_bindot()`.
      Caused by error in `compute_group()`:
      ! `weight` must be nonnegative integers, not a double vector.
    Code
      p <- ggplot(mtcars, aes(x = mpg, weight = -gear)) + geom_dotplot()
      e <- ggplot_build(p)
    Message
      Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.
    Condition
      Warning:
      Computation failed in `stat_bindot()`.
      Caused by error in `compute_group()`:
      ! `weight` must be nonnegative integers, not a double vector.

