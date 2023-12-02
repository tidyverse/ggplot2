# aes evaluation fails with unknown input

    Code
      is_calculated(environment())
    Condition
      Error in `is_calculated()`:
      ! Unknown input: <environment>
    Code
      strip_dots(environment())
    Condition
      Error in `strip_dots()`:
      ! Unknown input: <environment>

# aes() supports `!!!` in named arguments (#2675)

    Code
      aes(y = 1, !!!list(y = 2))
    Condition
      Error in `aes()`:
      ! formal argument "y" matched by multiple actual arguments

# alternative_aes_extract_usage() can inspect the call

    Code
      alternative_aes_extract_usage(x)
    Condition
      Error in `alternative_aes_extract_usage()`:
      ! Don't know how to get alternative usage for `foo`.

# new_aes() checks its inputs

    Code
      new_aes(1:5)
    Condition
      Error in `new_aes()`:
      ! `x` must be a <list>, not an integer vector.

