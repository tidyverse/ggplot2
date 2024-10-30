# accessing an undefined variable results in an error

    Code
      get_layer_data(p)
    Condition
      Error in `geom_point()`:
      ! Problem while computing aesthetics.
      i Error occurred in the 1st layer.
      Caused by error:
      ! object 'foo' not found

# aes standardises aesthetic names

    Duplicated aesthetics after name standardisation: colour

# warn_for_aes_extract_usage() warns for discouraged uses of $ and [[ within aes()

    Use of `df$x` is discouraged.
    i Use `x` instead.

---

    Use of `df[["x"]]` is discouraged.
    i Use `.data[["x"]]` instead.

---

    Use of `df$x` is discouraged.
    i Use `x` instead.

# warn_for_aes_extract_usage() does not evaluate function calls

    Use of `df$x` is discouraged.
    i Use `x` instead.

# Warnings are issued when plots use discouraged extract usage within aes()

    Use of `df$x` is discouraged.
    i Use `x` instead.

# aes evaluation fails with unknown input

    Unknown input: <environment>

---

    Unknown input: <environment>

# aes() supports `!!!` in named arguments (#2675)

    formal argument "y" matched by multiple actual arguments

# alternative_aes_extract_usage() can inspect the call

    Don't know how to get alternative usage for `foo`.

# new_aes() checks its inputs

    `x` must be a <list>, not an integer vector.

