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

