# stat_density works in both directions

    Code
      ggplot_build(p)
    Condition
      Error in `stat_density()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_density()` requires an x or y aesthetic.

# precompute_bandwidth() errors appropriately

    Code
      precompute_bw(1:10, bw = "foobar")
    Condition
      Error in `precompute_bw()`:
      ! `bw` must be one of "nrd0", "nrd", "ucv", "bcv", "sj", "sj-ste", or "sj-dpi", not "foobar".

---

    Code
      precompute_bw(1:10, bw = Inf)
    Condition
      Error in `precompute_bw()`:
      ! `bw` must be a finite, positive number, not `Inf`.

