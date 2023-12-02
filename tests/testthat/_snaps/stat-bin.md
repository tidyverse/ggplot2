# stat_bin throws error when wrong combination of aesthetic is present

    Code
      ggplot_build(ggplot(dat) + stat_bin())
    Condition
      Error in `stat_bin()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_bin()` requires an x or y aesthetic.

---

    Code
      ggplot_build(ggplot(dat, aes(x, y)) + stat_bin())
    Condition
      Error in `stat_bin()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_bin()` must only have an x or y aesthetic.

---

    Code
      ggplot_build(ggplot(dat, aes(x)) + stat_bin(y = 5))
    Condition
      Error in `stat_bin()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_bin()` requires a continuous x aesthetic.
      x the x aesthetic is discrete.
      i Perhaps you want `stat="count"`?

# inputs to binning are checked

    Code
      r <- comp_bin(dat, breaks = letters)
    Condition
      Warning:
      Computation failed in `stat_bin()`.
      Caused by error in `bins()`:
      ! `breaks` must be a <numeric> vector, not a character vector.
    Code
      r <- comp_bin(dat, binwidth = letters)
    Condition
      Warning:
      Computation failed in `stat_bin()`.
      Caused by error in `bin_breaks_width()`:
      ! `binwidth` must be a number, not a character vector.
    Code
      r <- comp_bin(dat, binwidth = -4)
    Condition
      Warning:
      Computation failed in `stat_bin()`.
      Caused by error in `bin_breaks_width()`:
      ! `binwidth` must be a number larger than or equal to 0, not the number -4.
    Code
      r <- comp_bin(dat, bins = -4)
    Condition
      Warning:
      Computation failed in `stat_bin()`.
      Caused by error in `bin_breaks_bins()`:
      ! `bins` must be a whole number larger than or equal to 1, not the number -4.

---

    Code
      bin_breaks_bins(3)
    Condition
      Error in `bin_breaks_bins()`:
      ! `x_range` must have two elements.

# stat_count throws error when both x and y aesthetic present

    Code
      ggplot_build(ggplot(dat, aes(x, y)) + stat_count())
    Condition
      Error in `stat_count()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_count()` must only have an x or y aesthetic.

