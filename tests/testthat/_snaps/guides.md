# Using non-position guides for position scales results in an informative error

    Code
      p <- ggplot(mpg, aes(cty, hwy)) + geom_point() + scale_x_continuous(guide = guide_legend())
      e <- ggplot_build(p)
    Condition
      Warning:
      `guide_legend()` cannot be used for x, xmin, xmax, or xend.
      i Use any non position aesthetic instead.

# guide specifications are properly checked

    Code
      validate_guide("test")
    Condition
      Error in `validate_guide()`:
      ! Unknown guide: test
    Code
      validate_guide(1)
    Condition
      Error in `validate_guide()`:
      ! Unknown guide: 1

---

    Code
      p <- ggplot(mtcars) + geom_point(aes(mpg, disp, shape = factor(gear))) + guides(
        shape = "colourbar")
      e <- ggplotGrob(p)
    Condition
      Warning:
      `guide_colourbar()` cannot be used for shape.
      i Use one of colour, color, or fill instead.

---

    Code
      guide_legend(title.position = "leftish")
    Condition
      Error in `guide_legend()`:
      ! `title.position` must be one of "top", "right", "bottom", or "left", not "leftish".
    Code
      guide_legend(label.position = "test")
    Condition
      Error in `guide_legend()`:
      ! `label.position` must be one of "top", "right", "bottom", or "left", not "test".
      i Did you mean "left"?
    Code
      guide_colourbar()$transform()
    Condition
      Error in `transform()`:
      ! `guide_colourbar()` does not implement a `transform()` method.
      i Did you mean to use `guide_axis()`?

---

    Code
      p <- ggplot(mtcars) + geom_point(aes(mpg, disp, colour = gear)) + guides(
        colour = guide_colourbar(label.position = "top"))
      ggplotGrob(p)
    Condition
      Error in `setup_params()`:
      ! When `direction` is "vertical", `label.position` must be one of "right" or "left", not "top".

---

    Code
      p <- ggplot(mtcars) + geom_point(aes(mpg, disp, colour = gear)) + guides(
        colour = guide_colourbar(direction = "horizontal", label.position = "left"))
      ggplotGrob(p)
    Condition
      Error in `setup_params()`:
      ! When `direction` is "horizontal", `label.position` must be one of "bottom" or "top", not "left".

---

    Code
      p <- ggplot(mtcars) + geom_point(aes(mpg, disp, colour = gear)) + guides(
        colour = guide_legend(nrow = 2, ncol = 2))
      ggplotGrob(p)
    Condition
      Error in `setup_params()`:
      ! `nrow` * `ncol` needs to be larger than the number of breaks (5).

# colorsteps and bins checks the breaks format

    Code
      p <- ggplot(mtcars) + geom_point(aes(mpg, disp, colour = paste("A", gear))) +
        guides(colour = "colorsteps")
      suppressWarnings(ggplotGrob(p))
    Condition
      Error in `parse_binned_breaks()`:
      ! Breaks are not formatted correctly for a bin legend.
      i Use `(<lower>, <upper>]` format to indicate bins.
    Code
      p <- ggplot(mtcars) + geom_point(aes(mpg, disp, colour = paste("A", gear))) +
        guides(colour = "bins")
      suppressWarnings(ggplotGrob(p))
    Condition
      Error in `parse_binned_breaks()`:
      ! Breaks are not formatted correctly for a bin legend.
      i Use `(<lower>, <upper>]` format to indicate bins.

# guide_axis_logticks calculates appropriate ticks

    Code
      res <- train_guide(guide, scale)$logkey
    Condition
      Warning:
      The `prescale_base` argument will override the scale's log-10 transformation in log-tick positioning.

# binning scales understand the different combinations of limits, breaks, labels, and show.limits

    Code
      res <- ggplotGrob(p + scale_color_binned(labels = 1:4, show.limits = TRUE,
      guide = "bins"))
    Condition
      Warning:
      `show.limits` is ignored when `labels` are given as a character vector.
      i Either add the limits to `breaks` or provide a function for `labels`.

---

    Code
      res <- ggplotGrob(p + scale_color_binned(labels = 1:4, show.limits = TRUE))
    Condition
      Warning:
      `show.limits` is ignored when `labels` are given as a character vector.
      i Either add the limits to `breaks` or provide a function for `labels`.

# a warning is generated when guides(<scale> = FALSE) is specified

    Code
      e <- ggplot_build(p)
    Condition
      Warning:
      The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in ggplot2 3.3.4.
      i Please use "none" instead.

# old S3 guides can be implemented

    Code
      expect_doppelganger("old S3 guide drawing a circle", ggplot(mtcars, aes(disp,
        mpg)) + geom_point() + guides(x = "circle"))
    Condition
      Warning:
      The S3 guide system was deprecated in ggplot2 3.5.0.
      i It has been replaced by a ggproto system that can be extended.

