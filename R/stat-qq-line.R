#' A quantile-quantile line
#'
#' @section Aesthetics:
#' \aesthetics{stat}{qq}
#' \aesthetics{geom}{path}
#'
#' @param distribution Distribution function to use, if x not specified
#' @param dparams Additional parameters passed on to \code{distribution}
#'   function.
#' @param line.p Vector of quantiles to use when fitting the Q-Q line, defaults
#' defaults to \code{c(.25, .75)}.
#' @param line.expand Vector of additive expansion factors along the x-axis,
#' defaults to \code{c(-.1, .1)}.
#' @inheritParams layer
#' @inheritParams geom_path
#' @section Computed variables:
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
geom_qq_line <- function(mapping = NULL,
                         data = NULL,
                         geom = "path",
                         position = "identity",
                         ...,
                         distribution = stats::qnorm,
                         dparams = list(),
                         line.p = c(.25, .75),
                         line.expand = c(-.1, .1),
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
      line.p = line.p,
      line.expand = line.expand,
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
 default_aes = aes(x = ..x.., y = ..y..),

 required_aes = c("sample"),

 compute_group = function(data,
                          scales,
                          quantiles = NULL,
                          distribution = stats::qnorm,
                          dparams = list(),
                          na.rm = FALSE,
                          line.p = c(.25, .75),
                          line.expand = c(-.1, .1)) {

   sample <- sort(data$sample)
   n <- length(sample)

   # Compute theoretical quantiles
   if (is.null(quantiles)) {
     quantiles <- stats::ppoints(n)
   } else {
     stopifnot(length(quantiles) == n)
   }

   theoretical <- do.call(distribution,
                          c(list(p = quote(quantiles)),
                            dparams))

   if (length(line.p) != 2) {
     stop("Cannot fit line quantiles ", line.p,
          ". Parameter line.p must have length 2.",
          call = FALSE)
   }

   x_coords <- do.call(distribution, c(list(p = line.p), dparams))
   y_coords <- quantile(sample, line.p)
   slope = diff(y_coords)/diff(x_coords)
   intercept = y_coords[1L] - slope * x_coords[1L]

   if (length(line.expand) != 2) {
     stop("Parameter line.expand must have length 2.", call = FALSE)
   }

   out <- data.frame(x = c(min(theoretical) + line.expand[1L],
                           max(theoretical) + line.expand[2L]))
   out$y <- slope * out$x + intercept

   out

 }
)
