# alt text can take a function

    Code
      get_alt_text(p)
    Output
      [1] "A plot showing class on a discrete x-axis and count on a continuous y-axis using a bar layer."

# get_alt_text checks dots

    Arguments in `...` must be used.
    x Problematic argument:
    * foo = "bar"
    i Did you misspell an argument name?

# warnings are thrown for unknown labels

    Ignoring unknown labels:
    * `foo = "bar"`

# plot.tag.position rejects invalid input

    The `plot.tag.position` theme element must be a <character/numeric/integer> object.

---

    `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "foobar".

---

    Code
      ggplotGrob(p + theme(plot.tag.position = c(0, 0.5, 1)))
    Condition
      Error in `theme()`:
      ! A <numeric> `plot.tag.position` must be a vector of length 2, not length 3.

---

    Code
      ggplotGrob(p + theme(plot.tag.position = c(0, 0), plot.tag.location = "margin"))
    Condition
      Error in `theme()`:
      ! A <numeric> `plot.tag.position` cannot be used with `"margin"` as `plot.tag.location`.

