# stat_bin throws error when wrong combination of aesthetic is present

    Problem while computing stat.
    i Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! `stat_bin()` requires an x or y aesthetic.

---

    Problem while computing stat.
    i Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! `stat_bin()` must only have an x or y aesthetic.

---

    Problem while computing stat.
    i Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! `stat_bin()` requires a continuous x aesthetic.
    x the x aesthetic is discrete.
    i Perhaps you want `stat="count"`?

# inputs to binning are checked

    `breaks` must be a <numeric> vector, not a character vector.

---

    `binwidth` must be a number, not a character vector.

---

    `binwidth` must be a number larger than or equal to 0, not the number -4.

---

    `bins` must be a whole number larger than or equal to 1, not the number -4.

# setting boundary and center

    Computation failed in `stat_bin()`.
    Caused by error in `compute_bins()`:
    ! Only one of `boundary` and `center` may be specified.

# bin errors at high bin counts

    Code
      compute_bins(c(1, 2e+06), binwidth = 1)
    Condition
      Error in `bin_breaks_width()`:
      ! The number of histogram bins must be less than 1,000,000.
      i Did you make `binwidth` too small?

# stat_count throws error when both x and y aesthetic present

    Problem while computing stat.
    i Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! `stat_count()` must only have an x or y aesthetic.

