# layers with empty data are silently omitted with facet_wrap

    Code
      get_layer_data(d)
    Condition
      Error in `combine_vars()`:
      ! Faceting variables must have at least one value.

# layers with empty data are silently omitted with facet_grid

    Code
      get_layer_data(d)
    Condition
      Error in `combine_vars()`:
      ! Faceting variables must have at least one value.

# Should error when totally empty data frame because there's no x and y

    Code
      get_layer_data(d)
    Condition
      Error in `geom_point()`:
      ! Problem while computing aesthetics.
      i Error occurred in the 2nd layer.
      Caused by error:
      ! object 'wt' not found

