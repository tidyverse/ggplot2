# axis_label_element_overrides errors when angles are outside the range [0, 90]

    Unrecognized `axis_position`: "test"
    i Use one of "top", "bottom", "left" or "right"

# Using non-position guides for position scales results in an informative error

    Guide with class <guide/legend> does not implement `guide_transform()`
    i Did you mean to use `guide_axis()`?

# guide specifications are properly checked

    object 'guide_test' of mode 'function' was not found

---

    Unknown guide: 1

---

    Guide `colorbar` cannot be used for shape.

---

    Title position "leftish" is invalid
    i Use one of "top", "bottom", "left", or "right"

---

    Guide with class <guide/colorbar> does not implement `guide_transform()`
    i Did you mean to use `guide_axis()`?

---

    label position "top" is invalid
    i use either "'left'" or "'right'"

---

    label position "left" is invalid
    i use either "'top'" or "'bottom'"

---

    label position `test` is invalid

---

    `nrow` * `ncol` needs to be larger than the number of breaks (5)

# colorsteps and bins checks the breaks format

    Breaks not formatted correctly for a bin legend.
    i Use `(<lower>, <upper>]` format to indicate bins

---

    Breaks not formatted correctly for a bin legend.
    i Use `(<lower>, <upper>]` format to indicate bins

# binning scales understand the different combinations of limits, breaks, labels, and show.limits

    `show.limits` is ignored when `labels` are given as a character vector
    i Either add the limits to `breaks` or provide a function for `labels`

---

    `show.limits` is ignored when `labels` are given as a character vector
    i Either add the limits to `breaks` or provide a function for `labels`

# a warning is generated when guides(<scale> = FALSE) is specified

    The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in ggplot2 3.3.4.
    i Please use "none" instead.

