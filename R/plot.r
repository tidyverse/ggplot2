#' Create a new ggplot plot
#'
#' \code{ggplot()} initializes a ggplot object. It can be used to
#' declare the input data frame for a graphic and to specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' \code{ggplot()} is typically used to construct a plot
#' incrementally, using the + operator to add layers to the
#' existing ggplot object. This is advantageous in that the
#' code is explicit about which layers are added and the order
#' in which they are added. For complex graphics with multiple
#' layers, initialization with \code{ggplot} is recommended.
#'
#' There are three common ways to invoke \code{ggplot}:
#' \itemize{
#'    \item \code{ggplot(df, aes(x, y, <other aesthetics>))}
#'    \item \code{ggplot(df)}
#'    \item \code{ggplot()}
#'   }
#' The first method is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used to add a layer using data from another
#' data frame. See the first example below. The second
#' method specifies the default data frame to use for the plot,
#' but no aesthetics are defined up front. This is useful when
#' one data frame is used predominantly as layers are added,
#' but the aesthetics may vary from one layer to another. The
#' third method initializes a skeleton \code{ggplot} object which
#' is fleshed out as layers are added. This method is useful when
#' multiple data frames are used to produce different layers, as
#' is often the case in complex graphics.
#'
#' The examples below illustrate how these methods of
#' invoking \code{ggplot} can be used in constructing a
#' graphic.
#' @seealso \url{http://had.co.nz/ggplot2}
#' @export
#' @keywords internal
#' @param data default data set
#' @param ... other arguments passed to specific methods
#' @examples
#
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
#'                  y = rnorm(30))
#' # Compute sample mean and standard deviation in each group
#' library(plyr)
#' ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))
#'
#' # Declare the data frame and common aesthetics.
#' # The summary data frame ds is used to plot
#' # larger red points in a second geom_point() layer.
#' # If the data = argument is not specified, it uses the
#' # declared data frame from ggplot(); ditto for the aesthetics.
#' ggplot(df, aes(x = gp, y = y)) +
#'    geom_point() +
#'    geom_point(data = ds, aes(y = mean),
#'               colour = 'red', size = 3)
#' # Same plot as above, declaring only the data frame in ggplot().
#' # Note how the x and y aesthetics must now be declared in
#' # each geom_point() layer.
#' ggplot(df) +
#'    geom_point(aes(x = gp, y = y)) +
#'    geom_point(data = ds, aes(x = gp, y = mean),
#'                  colour = 'red', size = 3)
#' # Set up a skeleton ggplot object and add layers:
#' ggplot() +
#'   geom_point(data = df, aes(x = gp, y = y)) +
#'   geom_point(data = ds, aes(x = gp, y = mean),
#'                         colour = 'red', size = 3) +
#'   geom_errorbar(data = ds, aes(x = gp, y = mean,
#'                     ymin = mean - sd, ymax = mean + sd),
#'                     colour = 'red', width = 0.4)
ggplot <- function(data = NULL, ...) UseMethod("ggplot")

#' @export
ggplot.default <- function(data = NULL, mapping = aes(), ...) {
  ggplot.data.frame(fortify(data, ...), mapping)
}

#' Reports whether x is a ggplot object
#' @param x An object to test
#' @export
is.ggplot <- function(x) inherits(x, "ggplot")

#' Create a new ggplot plot from a data frame
#'
#' @param data default data frame for plot
#' @param mapping default list of aesthetic mappings (these can be colour,
#'   size, shape, line type -- see individual geom functions for more details)
#' @param ... ignored
#' @param environment in which evaluation of aesthetics should occur
#' @seealso \url{http://had.co.nz/ggplot2}
#' @method ggplot data.frame
#' @export
ggplot.data.frame <- function(data, mapping=aes(), ..., environment = globalenv()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) stop("Mapping should be created with aes or aes_string")

  p <- structure(list(
    data = data,
    layers = list(),
    scales = Scales$new(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(),
    facet = facet_null(),
    plot_env = environment
  ), class = c("gg", "ggplot"))

  p$labels <- make_labels(mapping)

  set_last_plot(p)
  p
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p$layers <- lapply(plot$layers, function(x) x$clone())

  p
}
