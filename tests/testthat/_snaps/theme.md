# modifying theme element properties with + operator works

    Code
      theme_grey() + "asdf"
    Condition
      Error:
      ! Can't add `"asdf"` to a theme object.

# replacing theme elements with %+replace% operator works

    Code
      theme_grey() + "asdf"
    Condition
      Error:
      ! Can't add `"asdf"` to a theme object.

# theme validation happens at build stage

    The `text` theme element must be a <element_text> object.

---

    Theme element `text` must have class <element_text>.

# incorrect theme specifications throw meaningful errors

    Can't merge the `line` theme element.
    Caused by error in `merge_element()`:
    ! Only elements of the same class can be merged.

---

    Theme element `line` must have class <element_line>.

---

    Theme element `test` has `NULL` property without default: fill, colour, linewidth, and linetype.

---

    `new` must be a <theme> object, not the string "foo".

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

# elements can be merged

    Code
      merge_element(text_base, rect_base)
    Condition
      Error in `merge_element()`:
      ! Only elements of the same class can be merged.

# Theme elements are checked during build

    `plot.title.position` must be one of "panel" or "plot", not "test".

---

    `plot.caption.position` must be one of "panel" or "plot", not "test".

---

    `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "test".
    i Did you mean "left"?

# subtheme functions rename arguments as intended

    Ignoring unknown `theme()` elements: foo and bar.

# Theme validation behaves as expected

    The `aspect.ratio` theme element must be a <numeric/integer> object.

# theme() warns about conflicting palette options

    The `options('ggplot2.discrete.colour')` setting is incompatible with the `palette.colour.discrete` theme setting.
    i You can set `options(ggplot2.discrete.colour = NULL)`.

