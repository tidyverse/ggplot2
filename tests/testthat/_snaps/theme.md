# theme validation happens at build stage

    The `text` theme element must be a <element_text> object.

---

    Theme element `text` must have class <element_text>

# incorrect theme specifications throw meaningful errors

    Problem merging the `line` theme element
    Caused by error in `merge_element()`:
    ! Only elements of the same class can be merged

---

    Theme element `line` must have class <element_line>

---

    Theme element `test` has "NULL" property without default: fill, colour, linewidth, and linetype

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

# Theme elements are checked during build

    `plot.title.position` should be either "\"panel\"" or "\"plot\"".

---

    `plot.caption.position` should be either "\"panel\"" or "\"plot\"".

---

    `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "test".
    i Did you mean "left"?

