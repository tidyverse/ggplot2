# ggplot() throws informative errors

    `mapping` must be created with `aes()`.
    x You've supplied a character vector.

---

    `data` cannot be a function.
    i Have you misspelled the `data` argument in `ggplot()`

---

    Arguments in `...` must be used.
    x Problematic argument:
    * foobar = "nonsense"
    i Did you misspell an argument name?

# construction have user friendly errors

    Cannot use `+` with a single argument.
    i Did you accidentally put `+` on a new line?

---

    Cannot add <ggproto> objects together.
    i Did you forget to add this object to a <ggplot> object?

---

    Can't add `1` to a <ggplot> object.

---

    Can't add `geom_point` to a <ggplot> object
    i Did you forget to add parentheses, as in `geom_point()`?

