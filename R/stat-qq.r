#' Calculation for quantile-quantile plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "qq")}
#'
#' @param distribution Distribution function to use, if x not specified
#' @param dparams Additional parameters passed on to \code{distribution}
#'   function.
#' @inheritParams layer
#' @inheritParams geom_point
#' @section Computed variables:
#' \describe{
#'   \item{sample}{sample quantiles}
#'   \item{theoretical}{theoretical quantiles}
#' }
#' @export
#' @examples
#' \donttest{
#' df <- data.frame(y = rt(200, df = 5))
#' p <- ggplot(df, aes(sample = y))
#' p + stat_qq()
#' p + geom_point(stat = "qq")
#'
#' # Use fitdistr from MASS to estimate distribution params
#' params <- as.list(MASS::fitdistr(df$y, "t")$estimate)
#' ggplot(df, aes(sample = y)) +
#'   stat_qq(distribution = qt, dparams = params["df"])
#'
#' # Using to explore the distribution of a variable
#' ggplot(mtcars) +
#'   stat_qq(aes(sample = mpg))
#' ggplot(mtcars) +
#'   stat_qq(aes(sample = mpg, colour = factor(cyl)))
#' }
stat_qq <- function(mapping = NULL, data = NULL,
                    geom = "point", position = "identity",
                    ...,
                    distribution = stats::qnorm,
                    dparams = list(),
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatQq,
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
#' @rdname stat_qq
geom_qq <- stat_qq

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQq <- ggproto("StatQq", Stat,
  default_aes = aes(y = ..sample.., x = ..theoretical..),

  required_aes = c("sample"),

  compute_group = function(data, scales, quantiles = NULL,
                           distribution = stats::qnorm, dparams = list(),
                           na.rm = FALSE) {

    sample <- sort(data$sample)
    n <- length(sample)

    # Compute theoretical quantiles
    if (is.null(quantiles)) {
      quantiles <- stats::ppoints(n)
    } else {
      stopifnot(length(quantiles) == n)
    }

    theoretical <- do.call(distribution, c(list(p = quote(quantiles)), dparams))

    data.frame(sample, theoretical)
  }
)
