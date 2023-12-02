# ggplot() throws informative errors

    Code
      ggplot(mapping = letters)
    Condition
      Error in `ggplot()`:
      ! `mapping` should be created with `aes()`.
      x You've supplied a <character> object.

---

    Code
      ggplot(data)
    Condition
      Error in `ggplot()`:
      ! `data` cannot be a function.
      i Have you misspelled the `data` argument in `ggplot()`

# construction have user friendly errors

    Code
      +geom_point()
    Condition
      Error:
      ! Cannot use `+` with a single argument.
      i Did you accidentally put `+` on a new line?

---

    Code
      geom_point() + geom_bar()
    Condition
      Error:
      ! Cannot add <ggproto> objects together.
      i Did you forget to add this object to a <ggplot> object?

---

    Code
      ggplot() + 1
    Condition
      Error in `ggplot_add()`:
      ! Can't add `1` to a <ggplot> object.

---

    Code
      ggplot() + geom_point
    Condition
      Error in `ggplot_add()`:
      ! Can't add `geom_point` to a <ggplot> object
      i Did you forget to add parentheses, as in `geom_point()`?

