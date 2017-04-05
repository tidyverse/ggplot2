#' A quantile-quantile line
#'
#' @section Aesthetics:
#' \aesthetics{stat}{qq}
#' \aesthetics{geom}{abline}
#'
#' @param distribution Distribution function to use, if x not specified
#' @param dparams Additional parameters passed on to \code{distribution}
#'   function.
#' @inheritParams layer
#' @inheritParams geom_abline
#' @section Computed variables:
#' \describe{
#'   \item{slope}{slope of the line connecting the points at the the first
#'                and third quartiles of the theoretical and the sample
#'                distributions}
#'   \item{intercept}{intercept of the same line}
#' }
#' @export
#' @examples
#' \donttest{
#' df <- data.frame(y = rt(200, df = 5))
#' p <- ggplot(df, aes(sample = y))
#' p + stat_qq() + stat_qq_line()
#'
#' # Use fitdistr from MASS to estimate distribution params
#' params <- as.list(MASS::fitdistr(df$y, "t")$estimate)
#' ggplot(df, aes(sample = y)) +
#'   stat_qq(distribution = qt, dparams = params["df"]) +
#'   stat_qq_line(distribution = qt, dparams = params["df"])
#'
#' # Using to explore the distribution of a variable
#' ggplot(mtcars, aes(sample = mpg)) +
#'   stat_qq() +
#'   stat_qq_line()
#' ggplot(mtcars, aes(sample = mpg, colour = factor(cyl))) +
#'   stat_qq() +
#'   stat_qq_line()
#' }
geom_qq_line <- function(mapping = NULL, data = NULL,
                         geom = "abline", position = "identity",
                         ...,
                         distribution = stats::qnorm,
                         dparams = list(),
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatQqLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      distribution = distribution,
      dparams = dparams,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_qq_line
stat_qq_line <- geom_qq_line

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQqLine <- ggproto("StatQqLine", Stat,
 default_aes = aes(slope = ..slope.., intercept = ..intercept..),

 required_aes = c("sample"),

 compute_group = function(data, scales, quantiles = NULL,
                          distribution = stats::qnorm,
                          dparams = list(),
                          na.rm = FALSE) {

   sample <- sort(data$sample)
   n <- length(sample)

   # Compute theoretical quantiles
   if (is.null(quantiles)) {
     quantiles <- stats::ppoints(n)
   } else {
     stopifnot(length(quantiles) == n)
   }

   theoretical <- do.call(distribution,
                          c(list(p = quote(quantiles)), dparams))

   x <- do.call(distribution, c(list(p = c(.25, .75)), dparams))
   y <- quantile(sample, c(.25, .75))

   slope = diff(y)/diff(x)

   intercept = y[1L] - slope * x[1L]

   data.frame(slope, intercept)
 }
)
