# check_required_aesthetics() errors on missing

    `test()` requires the following missing aesthetics: y.

---

    `test()` requires the following missing aesthetics: x and y.

---

    `test()` requires the following missing aesthetics: x or y.

---

    `test()` requires the following missing aesthetics: fill and x or y.

# remove_missing checks input

    `na.rm` must be `TRUE` or `FALSE`, not an integer vector.

# characters survive remove_missing

    Removed 1 row containing non-finite outside the scale range.

# tolower() and toupper() has been masked

    Please use `to_lower_ascii()`, which works fine in all locales.

---

    Please use `to_upper_ascii()`, which works fine in all locales.

# parse_safe() checks input

    `text` must be a character vector, not an integer vector.

# width_cm() and height_cm() checks input

    Don't know how to get width of <character> object

---

    Don't know how to get height of <character> object

# cut_*() checks its input and output

    Insufficient data values to produce 10 bins.

---

    Specify exactly one of `n` and `width`.

---

    Only one of `boundary` and `center` may be specified.

# summary method gives a nice summary

    Code
      summary(p)
    Output
      data: manufacturer, model, displ, year, cyl, trans, drv, cty, hwy, fl,
        class [234x11]
      mapping:  x = ~displ, y = ~hwy, colour = ~drv
      scales:   x, xmin, xmax, xend, xintercept, xmin_final, xmax_final, xlower, xmiddle, xupper, x0, colour 
      faceting:  ~year, ~cyl 
      -----------------------------------
      geom_point: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      

