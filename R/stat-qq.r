#' A quantile-quantile plot
#'
#' `geom_qq()` and `stat_qq()` produce quantile-quantile plots. `geom_qq_line()` and
#' `stat_qq_line()` compute the slope and intercept of the line connecting the
#' points at specified quartiles of the theoretical and sample distributions.
#'
#' @eval rd_aesthetics("stat", "qq")
#' @eval rd_aesthetics("stat", "qq_line")
#' @param distribution Distribution function to use, if x not specified
#' @param dparams Additional parameters passed on to `distribution`
#'   function.
#' @inheritParams layer
#' @inheritParams geom_point
#' @section Computed variables:
#' Variables computed by `stat_qq()`:
#' \describe{
#'   \item{sample}{sample quantiles}
#'   \item{theoretical}{theoretical quantiles}
#' }
#' Variables computed by `stat_qq_line()`:
#' \describe{
#'   \item{x}{x-coordinates of the endpoints of the line segment connecting the
#'            points at the chosen quantiles of the theoretical and the sample
#'            distributions}
#'   \item{y}{y-coordinates of the endpoints}
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
geom_qq <- function(mapping = NULL, data = NULL,
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
    params = list2(
      distribution = distribution,
      dparams = dparams,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_qq
stat_qq <- geom_qq

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQq <- ggproto("StatQq", Stat,
  default_aes = aes(y = after_stat(sample), x = after_stat(theoretical)),

  required_aes = c("sample"),

  compute_group = function(self, data, scales, quantiles = NULL,
                           distribution = stats::qnorm, dparams = list(),
                           na.rm = FALSE) {

    sample <- sort(data$sample)
    n <- length(sample)

    # Compute theoretical quantiles
    if (is.null(quantiles)) {
      quantiles <- stats::ppoints(n)
    } else if (length(quantiles) != n) {
      cli::cli_abort("The length of {.arg quantiles} must match the length of the data")
    }

    theoretical <- inject(distribution(p = quantiles, !!!dparams))

    data_frame0(sample = sample, theoretical = theoretical)
  }
)
