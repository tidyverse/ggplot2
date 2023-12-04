#' Create a new ggplot
#'
#' `ggplot()` initializes a ggplot object. It can be used to
#' declare the input data frame for a graphic and to specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' `ggplot()` is used to construct the initial plot object,
#' and is almost always followed by a plus sign (`+`) to add
#' components to the plot.
#'
#' There are three common patterns used to invoke `ggplot()`:
#'
#' * `ggplot(data = df, mapping = aes(x, y, other aesthetics))`
#' * `ggplot(data = df)`
#' * `ggplot()`
#'
#' The first pattern is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used when adding a layer using data from another
#' data frame.
#'
#' The second pattern specifies the default data frame to use
#' for the plot, but no aesthetics are defined up front. This
#' is useful when one data frame is used predominantly for the
#' plot, but the aesthetics vary from one layer to another.
#'
#' The third pattern initializes a skeleton `ggplot` object, which
#' is fleshed out as layers are added. This is useful when
#' multiple data frames are used to produce different layers, as
#' is often the case in complex graphics.
#'
#' The `data =` and `mapping =` specifications in the arguments are optional
#' (and are often omitted in practice), so long as the data and the mapping
#' values are passed into the function in the right order. In the examples
#' below, however, they are left in place for clarity.
#'
#' @param data Default dataset to use for plot. If not already a data.frame,
#'   will be converted to one by [fortify()]. If not specified,
#'   must be supplied in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, must be supplied in each layer added to the plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment `r lifecycle::badge("deprecated")` Used prior to tidy
#'   evaluation.
#' @export
#' @examples
#' # Create a data frame with some sample data, then create a data frame
#' # containing the mean value for each group in the sample data.
#' set.seed(1)
#'
#' sample_df <- data.frame(
#'   group = factor(rep(letters[1:3], each = 10)),
#'   value = rnorm(30)
#' )
#'
#' group_means_df <- setNames(
#'   aggregate(value ~ group, sample_df, mean),
#'   c("group", "group_mean")
#' )
#'
#' # The following three code blocks create the same graphic, each using one
#' # of the three patterns specified above. In each graphic, the sample data
#' # are plotted in the first layer and the group means data frame is used to
#' # plot larger red points on top of the sample data in the second layer.
#'
#' # Pattern 1
#' # Both the `data` and `mapping` arguments are passed into the `ggplot()`
#' # call. Those arguments are omitted in the first `geom_point()` layer
#' # because they get passed along from the `ggplot()` call. Note that the
#' # second `geom_point()` layer re-uses the `x = group` aesthetic through
#' # that mechanism but overrides the y-position aesthetic.
#' ggplot(data = sample_df, mapping = aes(x = group, y = value)) +
#'   geom_point() +
#'   geom_point(
#'     mapping = aes(y = group_mean), data = group_means_df,
#'     colour = 'red', size = 3
#'   )
#'
#' # Pattern 2
#' # Same plot as above, passing only the `data` argument into the `ggplot()`
#' # call. The `mapping` arguments are now required in each `geom_point()`
#' # layer because there is no `mapping` argument passed along from the
#' # `ggplot()` call.
#' ggplot(data = sample_df) +
#'   geom_point(mapping = aes(x = group, y = value)) +
#'   geom_point(
#'     mapping = aes(x = group, y = group_mean), data = group_means_df,
#'     colour = 'red', size = 3
#'   )
#'
#' # Pattern 3
#' # Same plot as above, passing neither the `data` or `mapping` arguments
#' # into the `ggplot()` call. Both those arguments are now required in
#' # each `geom_point()` layer. This pattern can be particularly useful when
#' # creating more complex graphics with many layers using data from multiple
#' # data frames.
#' ggplot() +
#'   geom_point(mapping = aes(x = group, y = value), data = sample_df) +
#'   geom_point(
#'     mapping = aes(x = group, y = group_mean), data = group_means_df,
#'     colour = 'red', size = 3
#'   )
ggplot <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggplot")
}

#' @export
ggplot.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    cli::cli_abort(c(
      "{.arg mapping} must be created with {.fn aes}.",
      "x" = "You've supplied {.obj_type_friendly {mapping}}."
    ))
  }

  data <- fortify(data, ...)

  p <- structure(list(
    data = data,
    layers = list(),
    scales = scales_list(),
    guides = guides_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(default = TRUE),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot"))

  p$labels <- make_labels(mapping)

  set_last_plot(p)
  p
}

#' @export
ggplot.function <- function(data = NULL, mapping = aes(), ...,
                            environment = parent.frame()) {
  # Added to avoid functions end in ggplot.default
  cli::cli_abort(c(
    "{.arg data} cannot be a function.",
    "i" = "Have you misspelled the {.arg data} argument in {.fn ggplot}"
  ))
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}

#' Reports whether x is a ggplot object
#' @param x An object to test
#' @keywords internal
#' @export
is.ggplot <- function(x) inherits(x, "ggplot")

#' Explicitly draw plot
#'
#' Generally, you do not need to print or plot a ggplot2 plot explicitly: the
#' default top-level print method will do it for you. You will, however, need
#' to call `print()` explicitly if you want to draw a plot inside a
#' function or for loop.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @return Invisibly returns the original plot.
#' @export
#' @method print ggplot
#' @examples
#' colours <- list(~class, ~drv, ~fl)
#'
#' # Doesn't seem to do anything!
#' for (colour in colours) {
#'   ggplot(mpg, aes_(~ displ, ~ hwy, colour = colour)) +
#'     geom_point()
#' }
#'
#' # Works when we explicitly print the plots
#' for (colour in colours) {
#'   print(ggplot(mpg, aes_(~ displ, ~ hwy, colour = colour)) +
#'     geom_point())
#' }
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  set_last_plot(x)
  if (newpage) grid.newpage()

  # Record dependency on 'ggplot2' on the display list
  # (AFTER grid.newpage())
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )

  data <- ggplot_build(x)

  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }

  if (isTRUE(getOption("BrailleR.VI")) && rlang::is_installed("BrailleR")) {
    print(asNamespace("BrailleR")$VI(x))
  }

  invisible(x)
}
#' @rdname print.ggplot
#' @method plot ggplot
#' @export
plot.ggplot <- print.ggplot
