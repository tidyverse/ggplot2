# plot succeeds even if some computation fails

    Computation failed in `stat_summary()`.
    Caused by error in `fun()`:
    ! Failed computation

# error message is thrown when aesthetics are missing

    Code
      ggplot_build(p)
    Condition
      Error in `stat_sum()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `compute_layer()`:
      ! `stat_sum()` requires the following missing aesthetics: x and y.

# erroneously dropped aesthetics are found and issue a warning

    The following aesthetics were dropped during statistical transformation: fill.
    i This can happen when ggplot fails to infer the correct grouping structure in the data.
    i Did you forget to specify a `group` aesthetic or to convert a numerical variable into a factor?

---

    The following aesthetics were dropped during statistical transformation: colour and fill.
    i This can happen when ggplot fails to infer the correct grouping structure in the data.
    i Did you forget to specify a `group` aesthetic or to convert a numerical variable into a factor?

---

    The following aesthetics were dropped during statistical transformation: colour.
    i This can happen when ggplot fails to infer the correct grouping structure in the data.
    i Did you forget to specify a `group` aesthetic or to convert a numerical variable into a factor?

