# collide() checks the input data

    Code
      collide(data, width = 1, "test", pos_stack)
    Condition
      Error in `collide()`:
      ! y and ymax are undefined.
    Code
      data$y <- 1
      out <- collide(data, width = 2, "test", pos_stack)
    Condition
      Warning:
      `test()` requires non-overlapping x intervals.

