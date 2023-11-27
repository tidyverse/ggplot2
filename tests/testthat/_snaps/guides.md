# Using non-position guides for position scales results in an informative error

    `guide_legend()` cannot be used for x, xmin, xmax, or xend.
    i Use any non position aesthetic instead.

# guide specifications are properly checked

    Unknown guide: test

---

    Unknown guide: 1

---

    `guide_colourbar()` cannot be used for shape.
    i Use one of colour, color, or fill instead.

---

    `title.position` must be one of "top", "right", "bottom", or "left", not "leftish".

---

    `guide_colourbar()` does not implement a `transform()` method.
    i Did you mean to use `guide_axis()`?

---

    When `direction` is "vertical", `label.position` must be one of "right" or "left", not "top".

---

    When `direction` is "horizontal", `label.position` must be one of "bottom" or "top", not "left".

---

    `label.position` must be one of "top", "right", "bottom", or "left", not "test".
    i Did you mean "left"?

---

    `nrow` * `ncol` needs to be larger than the number of breaks (5)

# colorsteps and bins checks the breaks format

    Breaks are not formatted correctly for a bin legend.
    i Use `(<lower>, <upper>]` format to indicate bins.

---

    Breaks are not formatted correctly for a bin legend.
    i Use `(<lower>, <upper>]` format to indicate bins.

# guide_axis_logticks calculates appropriate ticks

    The `prescale_base` argument will override the scale's log-10 transformation in log-tick positioning.

# binning scales understand the different combinations of limits, breaks, labels, and show.limits

    `show.limits` is ignored when `labels` are given as a character vector.
    i Either add the limits to `breaks` or provide a function for `labels`.

---

    `show.limits` is ignored when `labels` are given as a character vector.
    i Either add the limits to `breaks` or provide a function for `labels`.

# a warning is generated when guides(<scale> = FALSE) is specified

    The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in ggplot2 3.3.4.
    i Please use "none" instead.

# old S3 guides can be implemented

    The S3 guide system was deprecated in ggplot2 3.5.0.
    i It has been replaced by a ggproto system that can be extended.

