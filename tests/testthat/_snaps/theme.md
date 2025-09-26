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
      Error in `method(merge_element, list(ggplot2::element, class_any))`:
      ! Only elements of the same class can be merged.

# margins() warn against wrong input lengths

    Code
      margin(c(1, 2), 3, 4, c(5, 6, 7))
    Condition
      Warning:
      In `margin()`, the arguments `t` and `l` should have length 1, not length 2 and 3.
      i Arguments get(s) truncated to length 1.
    Output
      [1] 1points 3points 4points 5points

# Theme elements are checked during build

    `plot.title.position` must be one of "panel" or "plot", not "test".

---

    `plot.caption.position` must be one of "panel" or "plot", not "test".

---

    `plot.tag.position` must be one of "topleft", "top", "topright", "left", "right", "bottomleft", "bottom", or "bottomright", not "test".
    i Did you mean "left"?

# subtheme functions rename arguments as intended

    Ignoring unknown `theme()` elements: foo and bar.

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

# Theme validation behaves as expected

    The `aspect.ratio` theme element must be a <numeric/integer> object.

# theme() warns about conflicting palette options

    The `options('ggplot2.discrete.colour')` setting is incompatible with the `palette.colour.discrete` theme setting.
    i You can set `options(ggplot2.discrete.colour = NULL)`.

# theme elements are covered in `theme_sub_*()` functions

    Code
      extra_elements
    Output
       [1] "..."                           "line"                         
       [3] "rect"                          "text"                         
       [5] "title"                         "point"                        
       [7] "polygon"                       "geom"                         
       [9] "spacing"                       "margins"                      
      [11] "aspect.ratio"                  "axis.text.theta"              
      [13] "axis.text.r"                   "axis.ticks.theta"             
      [15] "axis.ticks.r"                  "axis.minor.ticks.theta"       
      [17] "axis.minor.ticks.r"            "axis.ticks.length.theta"      
      [19] "axis.ticks.length.r"           "axis.minor.ticks.length.theta"
      [21] "axis.minor.ticks.length.r"     "axis.line.theta"              
      [23] "axis.line.r"                   "legend.just"                  
      [25] "legend.just.top"               "legend.just.bottom"           
      [27] "legend.just.left"              "legend.just.right"            
      [29] "legend.just.inside"            "complete"                     
      [31] "validate"                     

