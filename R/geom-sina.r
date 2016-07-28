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
#' @inheritParams stat_sina
#'
#' @details There are two available ways to define the x-axis borders for the
#' samples to spread within:
#' \itemize{
#'  \item{\code{method == "density"}
#'
#'    A density kernel is estimated along the y-axis for every sample group. The
#'    borders are then defined by the density curve. Tuning parameter
#'    \code{adjust} can be used to control the density bandwidth in the same way
#'    it is used in \code{\link[stats]{density}}. }
#'
#'  \item{\code{method == "counts"}:
#'
#'    The borders are defined by the number of samples that occupy the same bin.
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
#' ggplot(midwest, aes(state, area)) + geom_point()
#'
#' # Boxplot and Violin plots convey information on the distribution but not the
#' # number of samples, while Jitter does the opposite.
#' ggplot(midwest, aes(state, area)) + geom_violin()
#' ggplot(midwest, aes(state, area)) + geom_jitter()
#'
#' # Sina does both!
#' ggplot(midwest, aes(state, area)) + geom_violin() + geom_sina()
#'
#' p <- ggplot(midwest, aes(state, popdensity)) + scale_y_log10()
#' p + geom_sina()
#'
#' # Colour the points based on the data set's columns
#' p + geom_sina(aes(colour = inmetro))
#'
#' # Or any other way
#' cols <- midwest$popdensity > 10000
#' p + geom_sina(colour = cols + 1L)
#'
#' # Sina plots with continuous x:
#' p <- ggplot(midwest, aes(cut_width(area, 0.02), popdensity)) + scale_y_log10()
#' p + geom_sina()
#'
#'
#' ###Sample gaussian distributions
#' # Unimodal
#' a <- rnorm(500, 6, 1)
#' b <- rnorm(400, 5, 1.5)
#'
#' # Bimodal
#' c <- c(rnorm(200, 3, .7), rnorm(50, 7, 0.4))
#'
#' # Trimodal
#' d <- c(rnorm(200, 2, 0.7), rnorm(300, 5.5, 0.4), rnorm(100, 8, 0.4))
#'
#' df <- data.frame(
#'   "Distribution" = c(rep("Unimodal 1", length(a)),
#'                      rep("Unimodal 2", length(b)),
#'                      rep("Bimodal", length(c)),
#'                      rep("Trimodal", length(d))),
#'   "Value" = c(a, b, c, d))
#'
#' # Reorder levels
#' df$Distribution <- factor(df$Distribution,
#'                           levels(df$Distribution)[c(3, 4, 1, 2)])
#'
#' p <- ggplot(df, aes(Distribution, Value))
#' p + geom_boxplot()
#' p + geom_violin() + geom_sina()
#'
#' # By default, Sina plot scales the width of the class according to the width
#' # of the class with the highest density. Turn group-wise scaling off with:
#' p + geom_violin() + geom_sina(scale = FALSE)
geom_sina <- function(mapping = NULL, data = NULL,
                      stat = "sina", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
    layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSina,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

}

GeomSina <- ggproto("GeomSina", GeomPoint,

  setup_data = function(data, params) {
    transform(data, x = xend)
  }
)

