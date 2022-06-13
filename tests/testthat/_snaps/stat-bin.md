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
    ! `stat_bin()` requires a continuous x aesthetic
    x the x aesthetic is discrete.
    i Perhaps you want `stat="count"`?

# inputs to binning are checked

    Computation failed in `stat_bin()`
    Caused by error in `bins()`:
    ! `breaks` must be a numeric vector

---

    `x_range` must have two elements

---

    Computation failed in `stat_bin()`
    Caused by error in `bin_breaks_width()`:
    ! `width` must be a number

---

    Computation failed in `stat_bin()`
    Caused by error in `bin_breaks_width()`:
    ! `binwidth` must be positive

---

    `x_range` must have two elements

---

    Computation failed in `stat_bin()`
    Caused by error in `bin_breaks_bins()`:
    ! `bins` must be 1 or greater

# stat_count throws error when both x and y aesthetic present

    Problem while computing stat.
    i Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! `stat_count()` must only have an x or y aesthetic.

