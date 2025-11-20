# elements can be merged

    Code
      merge_element(text_base, rect_base)
    Condition
      Error in `method(merge_element, list(ggplot2::element, class_any))`:
      ! Only elements of the same class can be merged.

# element tree can be modified

    The `blablabla` theme element is not defined in the element hierarchy.

---

    The `blablabla` theme element must be a <character> object.

---

    The `blablabla` theme element must be a <unit> object.

---

    The `blablabla` theme element must be a <element_text> object.

---

    `element_tree` must have names.

---

    `element_tree` must have elements constructed with `el_def()`.
    i Invalid structure: "foo"

---

    Invalid parent in `element_tree`: "foo".

# element_text throws appropriate conditions

    Vectorized input to `element_text()` is not officially supported.
    i Results may be unexpected or may change in future versions of ggplot2.

---

    The `margin` argument should be constructed using the `margin()` function.

---

    Code
      element_text(margin = 5)
    Condition
      Error in `as_margin()`:
      ! `margin` must be a <margin> class, not a number.

---

    Code
      element_text(colour = sqrt(2))
    Condition
      Error:
      ! <ggplot2::element_text> object properties are invalid:
      - @colour cannot be a decimal number, but could be an integer.

---

    Code
      element_grob(el, label = element_blank())
    Condition
      Warning:
      `label` cannot be a <ggplot2::element_blank> object.
    Output
      zeroGrob[NULL] 

