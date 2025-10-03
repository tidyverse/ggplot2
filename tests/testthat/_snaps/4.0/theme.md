# modifying theme element properties with + operator works

    Code
      theme_grey() + "asdf"
    Condition
      Error in `method(+, list(ggplot2::theme, class_any))`:
      ! Can't add `"asdf"` to a theme object.

