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

# empty data overrides plot defaults

    Code
      get_layer_data(d)
    Condition
      Error in `geom_point()`:
      ! Problem while computing aesthetics.
      i Error occurred in the 2nd layer.
      Caused by error:
      ! object 'wt' not found

