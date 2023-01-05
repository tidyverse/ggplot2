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
#' There are three common patterns used to invoke `ggplot()`:
#'
#' * `ggplot(df, aes(x, y, other aesthetics))`
#' * `ggplot(df)`
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
#' plot, but the aesthetics may vary from one layer to another. 
#'
#' The third pattern initializes a skeleton `ggplot` object, which
#' is fleshed out as layers are added. This is useful when
#' multiple data frames are used to produce different layers, as
#' is often the case in complex graphics.
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
#' # containing the mean for each group in the sample data.
#' set.seed(1)
#' sample_df <- data.frame(
#'   group = factor(rep(letters[1:3], each = 10)),
#'   value = rnorm(30)
#' )
#' group_means_df <- do.call(rbind, lapply(split(sample_df, sample_df$group), function(d) {
#'   data.frame(group_mean = mean(d$value), group = d$group)
#' }))
#' 
#' # Pattern 1
#' # The group means data frame is used to plot larger red points on top
#' # of the sample data. Note that we don't need to supply `data =` or `mapping =`
#' # in each layer because the arguments are passed into ggplot() in the default
#' # positions.
#' ggplot(sample_df, aes(x = group, y = value)) +
#'   geom_point() +
#'   geom_point(group_means_df, aes(y = group_mean), colour = 'red', size = 3)
#' 
#' # Pattern 2
#' # Same plot as above, declaring only the data frame in ggplot().
#' # Note how the x and y aesthetics must now be declared in
#' # each geom_point() layer.
#' ggplot(sample_df) +
#'   geom_point(aes(x = group, y = value)) +
#'   geom_point(group_means_df, aes(x = group, y = group_mean), colour = 'red', size = 3)
#' 
#' # Pattern 3
#' # Alternatively, we can fully specify the plot in each layer. This
#' # can be particularly useful when working with complex, multi-dataset graphics.
#' ggplot() +
#'  geom_point(sample_df, aes(x = group, y = value)) +
#'  geom_point(group_means_df, aes(x = group, y = group_mean), colour = 'red', size = 3)
#' }
ggplot <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggplot")
}

#' @export
ggplot.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    cli::cli_abort(c(
      "{.arg mapping} should be created with {.fn aes}.",
      "x" = "You've supplied a {.cls {class(mapping)[1]}} object"
    ))
  }

  data <- fortify(data, ...)

  p <- structure(list(
    data = data,
    layers = list(),
    scales = scales_list(),
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
