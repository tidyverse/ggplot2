#' Sina plot
#'
#' The sina plot is a data visualization chart suitable for plotting any single
#' variable in a multiclass dataset. It is an enhanced jitter strip chart,
#' where the width of the jitter is controlled by the density distribution of
#' the data within each class.
#'
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams position_sina
#'
#' @details There are two available ways to define the x-axis borders for the
#' samples to spread within:
#' \itemize{
#'  \item{\code{method == "density"}
#'
#'   A density kernel is estimated along the y-axis for every sample group. The
#'   borders are then defined by the density curve. Tuning parameter
#'   \code{adjust} can be used to control the density bandwidth in the same way
#'   it is used in \code{\link[stats]{density}}. }
#'  \item{\code{method == "neighbourhood"}:
#'
#'  The borders are defined by the number of samples that 'live' in the same
#'  neighbourhood and the parameter \code{adjust} in the following fashion:
#'
#'  \code{x_border = nsamples * adjust}
#'
#'   }
#' }
#'
#' @seealso
#'  \code{\link{geom_point}} for regular, unjittered points,
#'
#'  \code{\link{geom_boxplot}} and \code{\link{geom_violin}} for other ways of
#'  looking at the conditional distribution of a variable.
#'
#' @references Sidiropoulos, N., Sohi, S. H., Rapin, N. and Bagger, F.O. (2015)
#' SinaPlot: an enhanced chart for simple and truthful representation of single
#' observations over multiple classes.
#' bioRxiv doi: \url{http://dx.doi.org/10.1101/028191}
#'
#' @export
#' @examples
#' p <- ggplot(mpg, aes(cyl, hwy))
#' p + geom_point()
#' p + geom_sina()
#'
#' # Add aesthetic mappings
#' p + geom_sina(aes(colour = class))
#'
#' # Use smaller width/height to emphasise categories
#' ggplot(mpg, aes(cyl, hwy)) + geom_sina()
#' ggplot(mpg, aes(cyl, hwy)) + geom_sina(scale = FALSE)
#'
geom_sina <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "sina",
                      ...,
                      binwidth = 0.02,
                      scale = TRUE,
                      neighbour_limit = 1,
                      method = NULL,
                      adjust = 1,
                      show.legend = NA,
                      inherit.aes = TRUE) {

  position <- position_sina(binwidth, scale, neighbour_limit, method, adjust)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ...
    )
  )
}
