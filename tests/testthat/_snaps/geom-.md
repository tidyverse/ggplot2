# aesthetic checking in geom throws correct errors

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_point()`:
      ! Problem while setting up geom aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `use_defaults()`:
      ! Aesthetic modifiers returned invalid values
      x The following mappings are invalid
      x `colour = after_scale(data)`
      i Did you map the modifier in the wrong layer?

---

    Code
      check_aesthetics(aes, 4)
    Condition
      Error in `check_aesthetics()`:
      ! Aesthetics must be either length 1 or the same as the data (4).
      x Fix the following mappings: `d` and `e`.

