# stat_density2d can produce contour and raster data

    Code
      ggplot_build(p + stat_density_2d(contour_var = "abcd"))
    Condition
      Error in `stat_density_2d()`:
      ! Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `compute_layer()`:
      ! `contour_var` must be one of "density", "ndensity", or "count", not "abcd".

