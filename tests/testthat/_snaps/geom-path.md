# geom_path() throws meaningful error on bad combination of varying aesthetics

    Problem while converting geom to grob.
    i Error occurred in the 1st layer.
    Caused by error in `draw_panel()`:
    ! `geom_path()` can't have varying colour, linewidth, and/or alpha along the line when linetype isn't solid.

# stairstep() exists with error when an invalid `direction` is given

    Code
      stairstep(df, direction = "invalid")
    Condition
      Error in `stairstep()`:
      ! `direction` must be one of "hv", "vh", or "mid", not "invalid".

# NA linetype is dropped with warning

    Removed 2 rows containing missing values or values outside the scale range (`geom_path()`).

