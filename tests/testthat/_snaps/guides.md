# guide specifications are properly checked

    Unknown guide: test

---

    Unknown guide: 1

---

    `guide_colourbar()` cannot be used for shape.
    i Use one of colour, color, or fill instead.

---

    `legend.title.position` must be one of "top", "right", "bottom", or "left", not "leftish".

---

    `guide_colourbar()` does not implement a `transform()` method.
    i Did you mean to use `guide_axis()`?

---

    When `direction` is "vertical", `legend.text.position` must be one of "right" or "left", not "top".

---

    When `direction` is "horizontal", `legend.text.position` must be one of "bottom" or "top", not "left".

---

    `legend.text.position` must be one of "top", "right", "bottom", or "left", not "test".
    i Did you mean "left"?

---

    `nrow` * `ncol` needs to be larger than the number of breaks (5).

# get_guide_data retrieves keys appropriately

    Code
      get_guide_data(b, 1)
    Condition
      Error in `get_guide_data()`:
      ! `aesthetic` must be a single string, not the number 1.

---

    Code
      get_guide_data(b, "x", panel = "a")
    Condition
      Error in `get_guide_data()`:
      ! `panel` must be a whole number, not the string "a".

# binning scales understand the different combinations of limits, breaks, labels, and show.limits

    `show.limits` is ignored when `labels` are given as a character vector.
    i Either add the limits to `breaks` or provide a function for `labels`.

---

    `show.limits` is ignored when `labels` are given as a character vector.
    i Either add the limits to `breaks` or provide a function for `labels`.

# guides() warns if unnamed guides are provided

    Guides provided to `guides()` must be named.
    i All guides are unnamed.

---

    Guides provided to `guides()` must be named.
    i The 2nd guide is unnamed.

# old S3 guides can be implemented

    The S3 guide system was deprecated in ggplot2 3.5.0.
    i It has been replaced by a ggproto system that can be extended.

