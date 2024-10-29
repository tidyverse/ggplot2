# stat_ecdf works in both directions

    Problem while computing stat.
    i Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! `stat_ecdf()` requires an x or y aesthetic.

# weighted ecdf warns about weird weights

    The weight aesthetic does not support non-finite or `NA` values.
    i These weights were replaced by "0".

---

    The sum of the weight aesthetic is close to "0".
    i Computed eCDF might be unstable.

---

    Code
      wecdf(1:10, rep(c(-1, 1), 5))
    Condition
      Error in `wecdf()`:
      ! Cannot compute eCDF when the weight aesthetic sums up to "0".

