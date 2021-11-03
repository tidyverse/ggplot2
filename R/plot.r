#' Create a new ggplot
#'
#' `ggplot()` initializes a ggplot object. It can be used to
#' declare the input data frame for a graphic and to specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' `ggplot()` is used to construct the initial plot object,
#' and is almost always followed by `+` to add component to the
#' plot. There are three common ways to invoke `ggplot()`:
#'
#' * `ggplot(df, aes(x, y, other aesthetics))`
#' * `ggplot(df)`
#' * `ggplot()`
#'
#' The first method is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used to add a layer using data from another
#' data frame. See the first example below. The second
#' method specifies the default data frame to use for the plot,
#' but no aesthetics are defined up front. This is useful when
#' one data frame is used predominantly as layers are added,
#' but the aesthetics may vary from one layer to another. The
#' third method initializes a skeleton `ggplot` object which
#' is fleshed out as layers are added. This method is useful when
#' multiple data frames are used to produce different layers, as
#' is often the case in complex graphics.
#'
#' @param data Default dataset to use for plot. If not already a data.frame,
#'   will be converted to one by [fortify()]. If not specified,
#'   must be supplied in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, must be supplied in each layer added to the plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment DEPRECATED. Used prior to tidy evaluation.
#' @export
#' @examples
#' # Generate some sample data, then compute mean and standard deviation
#' # in each group
#' df <- data.frame(
#'   gp = factor(rep(letters[1:3], each = 10)),
#'   y = rnorm(30)
#' )
#' ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
#'   data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)
#' }))
#'
#' # The summary data frame ds is used to plot larger red points on top
#' # of the raw data. Note that we don't need to supply `data` or `mapping`
#' # in each layer because the defaults from ggplot() are used.
#' ggplot(df, aes(gp, y)) +
#'   geom_point() +
#'   geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)
#'
#' # Same plot as above, declaring only the data frame in ggplot().
#' # Note how the x and y aesthetics must now be declared in
#' # each geom_point() layer.
#' ggplot(df) +
#'   geom_point(aes(gp, y)) +
#'   geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3)
#'
#' # Alternatively we can fully specify the plot in each layer. This
#' # is not useful here, but can be more clear when working with complex
#' # mult-dataset graphics
#' ggplot() +
#'   geom_point(data = df, aes(gp, y)) +
#'   geom_point(data = ds, aes(gp, mean), colour = 'red', size = 3) +
#'   geom_errorbar(
#'     data = ds,
#'     aes(gp, mean, ymin = mean - sd, ymax = mean + sd),
#'     colour = 'red',
#'     width = 0.4
#'   )
ggplot <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggplot")
}

#' @export
ggplot.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    abort("Mapping should be created with `aes()` or `aes_()`.")
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
  abort(glue("
    You're passing a function as global data.
    Have you misspelled the `data` argument in `ggplot()`
  "))
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
