# warnings are thrown when parameters cause mapping and data to be ignored

    Code
      out <- geom_vline(aes(), xintercept = 2)
    Condition
      Warning:
      `geom_vline()`: Ignoring `mapping` because `xintercept` was provided.
    Code
      out <- geom_vline(data = mtcars, xintercept = 2)
    Condition
      Warning:
      `geom_vline()`: Ignoring `data` because `xintercept` was provided.
    Code
      out <- geom_hline(aes(), yintercept = 2)
    Condition
      Warning:
      `geom_hline()`: Ignoring `mapping` because `yintercept` was provided.
    Code
      out <- geom_hline(data = mtcars, yintercept = 2)
    Condition
      Warning:
      `geom_hline()`: Ignoring `data` because `yintercept` was provided.
    Code
      out <- geom_abline(aes(), slope = 2)
    Condition
      Warning:
      `geom_abline()`: Ignoring `mapping` because `slope` and/or `intercept` were provided.
    Code
      out <- geom_abline(aes(), intercept = 2)
    Condition
      Warning:
      `geom_abline()`: Ignoring `mapping` because `slope` and/or `intercept` were provided.
    Code
      out <- geom_abline(data = mtcars, slope = 2)
    Condition
      Warning:
      `geom_abline()`: Ignoring `data` because `slope` and/or `intercept` were provided.
    Code
      out <- geom_abline(data = mtcars, intercept = 2)
    Condition
      Warning:
      `geom_abline()`: Ignoring `data` because `slope` and/or `intercept` were provided.

