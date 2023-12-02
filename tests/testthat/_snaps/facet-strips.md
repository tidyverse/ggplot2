# facet_grid() warns about bad switch input

    Code
      facet_grid(am ~ cyl, switch = "z")
    Condition
      Error in `facet_grid()`:
      ! `switch` must be one of "both", "x", or "y", not "z".

