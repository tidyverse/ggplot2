# theme validation happens at build stage

    Code
      print(p)
    Condition
      Error in `mapply()`:
      ! The `text` theme element must be a <element_text> object.

---

    Code
      print(p)
    Condition
      Error in `element_render()`:
      ! Theme element `text` must have class <element_text>.

# incorrect theme specifications throw meaningful errors

    Code
      add_theme(theme_grey(), theme(line = element_rect()))
    Condition
      Error:
      ! Can't merge the `line` theme element.
      Caused by error in `merge_element()`:
      ! Only elements of the same class can be merged.

---

    Code
      calc_element("line", theme(line = element_rect()))
    Condition
      Error:
      ! Theme element `line` must have class <element_line>.

---

    Code
      calc_element("test", theme_gray() + theme(test = element_rect()))
    Condition
      Error:
      ! Theme element `test` has `NULL` property without default: fill, colour, linewidth, and linetype.

---

    Code
      theme_set("foo")
    Condition
      Error in `theme_set()`:
      ! `new` must be a <theme> object, not the string "foo".

# element tree can be modified

    Code
      print(p)
    Condition
      Error in `mapply()`:
      ! The `blablabla` theme element is not defined in the element hierarchy.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `mapply()`:
      ! The `blablabla` theme element must be a <character> object.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `mapply()`:
      ! The `blablabla` theme element must be a <unit> object.

---

    Code
      ggplotGrob(p1)
    Condition
      Error in `mapply()`:
      ! The `blablabla` theme element must be a <element_text> object.

# Theme elements are checked during build

    Code
      ggplotGrob(p)
    Condition
      Error in `theme()`:
      ! `plot.title.position` must be one of "panel" or "plot", not "test".

---

    Code
      ggplotGrob(p)
    Condition
      Error in `theme()`:
      ! `plot.caption.position` must be one of "panel" or "plot", not "test".

---

    Code
      ggplotGrob(p)
    Condition
      Error in `theme()`:
      ! `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "test".
      i Did you mean "left"?

# Theme validation behaves as expected

    Code
      validate_element("A", "aspect.ratio", tree)
    Condition
      Error:
      ! The `aspect.ratio` theme element must be a <numeric/integer> object.

