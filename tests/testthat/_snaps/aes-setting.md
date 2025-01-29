# aesthetic parameters match length of data

    Code
      set_colours(rep("red", 2))
    Condition
      Error in `geom_point()`:
      ! Problem while setting up geom aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `check_aesthetics()`:
      ! Aesthetics must be either length 1 or the same as the data (5).
      x Fix the following mappings: `colour`.

---

    Code
      set_colours(rep("red", 3))
    Condition
      Error in `geom_point()`:
      ! Problem while setting up geom aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `check_aesthetics()`:
      ! Aesthetics must be either length 1 or the same as the data (5).
      x Fix the following mappings: `colour`.

---

    Code
      set_colours(rep("red", 4))
    Condition
      Error in `geom_point()`:
      ! Problem while setting up geom aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `check_aesthetics()`:
      ! Aesthetics must be either length 1 or the same as the data (5).
      x Fix the following mappings: `colour`.

