# stat_count() checks the aesthetics

    Code
      ggplot_build(p)
    Condition
      Error in `stat_count()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_count()` requires an x or y aesthetic.

---

    Code
      ggplot_build(p)
    Condition
      Error in `stat_count()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_count()` must only have an x or y aesthetic.

