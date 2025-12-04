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

# validate_guide finds guides with namespace prefixes

    Unknown guide: bar

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

# guides() warns if unnamed guides are provided

    Guides provided to `guides()` must be named.
    i All guides are unnamed.

---

    Guides provided to `guides()` must be named.
    i The 2nd guide is unnamed.

