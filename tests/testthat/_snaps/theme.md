# modifying theme element properties with + operator works

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
    Caused by error in `method(merge_element, list(ggplot2::element, class_any))`:
    ! Only elements of the same class can be merged.

---

    Theme element `line` must have class <element_line>.

---

    Theme element `test` has `NULL` property without default: fill, colour, linewidth, linetype, and linejoin.

---

    `new` must be a <theme> object, not the string "foo".

# Theme elements are checked during build

    `plot.title.position` must be one of "panel" or "plot", not "test".

---

    `plot.caption.position` must be one of "panel" or "plot", not "test".

---

    `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "test".
    i Did you mean "left"?

# Theme validation behaves as expected

    The `aspect.ratio` theme element must be a <numeric/integer> object.

# theme() warns about conflicting palette options

    The `options('ggplot2.discrete.colour')` setting is incompatible with the `palette.colour.discrete` theme setting.
    i You can set `options(ggplot2.discrete.colour = NULL)`.

# theme element conversion to lists works

    Unknown arguments to `element_text()`: `italic`, `fontweight`, and `fontwidth`.

# all expected theme elements are documented

    Code
      extra_elements
    Output
       [1] "strip.placement.x"            "strip.placement.y"           
       [3] "palette.colour.discrete"      "palette.colour.continuous"   
       [5] "palette.fill.discrete"        "palette.fill.continuous"     
       [7] "palette.alpha.discrete"       "palette.alpha.continuous"    
       [9] "palette.linewidth.discrete"   "palette.linewidth.continuous"
      [11] "palette.size.discrete"        "palette.size.continuous"     
      [13] "palette.shape.discrete"       "palette.shape.continuous"    
      [15] "palette.linetype.discrete"    "palette.linetype.continuous" 

