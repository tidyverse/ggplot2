# stat_boxplot drops missing rows with a warning

    Code
      res <- ggplot_build(p1)
    Condition
      Warning:
      Removed 10 rows containing missing values or values outside the scale range (`stat_boxplot()`).
    Code
      res <- ggplot_build(p2)
    Condition
      Warning:
      Removed 10 rows containing missing values or values outside the scale range (`stat_boxplot()`).

# stat_boxplot errors with missing x/y aesthetics

    Code
      ggplot_build(p)
    Condition
      Error in `geom_boxplot()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `setup_params()`:
      ! `stat_boxplot()` requires an x or y aesthetic.

