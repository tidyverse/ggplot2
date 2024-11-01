# names of values used in manual scales

    No shared levels found between `names(values)` of the manual scale and the data's colour values.

# insufficient values raise an error

    Code
      ggplot_build(p + scale_colour_manual(values = "black"))
    Condition
      Error in `palette()`:
      ! Insufficient values in manual scale. 2 needed but only 1 provided.

# fewer values (#3451)

    Code
      s2$map(c("4", "6", "8"))
    Condition
      Error in `palette()`:
      ! Insufficient values in manual scale. 3 needed but only 2 provided.

