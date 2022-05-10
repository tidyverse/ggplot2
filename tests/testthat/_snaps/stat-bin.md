# stat_bin throws error when wrong combination of aesthetic is present

    `stat_bin()` requires an x or y aesthetic.

---

    `stat_bin()` must only have an x or y aesthetic.

---

    `stat_bin()` requires a continuous x aesthetic
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

    `stat_count()` must only have an x or y aesthetic.

