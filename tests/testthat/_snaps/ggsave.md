# unknown device triggers error

    Code
      plot_dev(1)
    Condition
      Error:
      ! `device` must be a string, function or `NULL`, not the number 1.

# invalid single-string DPI values throw an error

    Code
      parse_dpi("abc")
    Condition
      Error:
      ! `dpi` must be one of "screen", "print", or "retina", not "abc".

# invalid non-single-string DPI values throw an error

    Code
      parse_dpi(factor(100))
    Condition
      Error:
      ! `dpi` must be a single number or string, not a <factor> object.
    Code
      parse_dpi(c("print", "screen"))
    Condition
      Error:
      ! `dpi` must be a single number or string, not a character vector.
    Code
      parse_dpi(c(150, 300))
    Condition
      Error:
      ! `dpi` must be a single number or string, not a double vector.
    Code
      parse_dpi(list(150))
    Condition
      Error:
      ! `dpi` must be a single number or string, not a list.

