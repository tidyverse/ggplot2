#' @rdname geom_qq
#' @export
#' @param line.p Vector of quantiles to use when fitting the Q-Q line, defaults
#' defaults to `c(.25, .75)`.
#' @param fullrange Should the q-q line span the full range of the plot, or just
#'   the data
geom_qq_line <- function(mapping = NULL,
                         data = NULL,
                         geom = "path",
                         position = "identity",
                         ...,
                         distribution = stats::qnorm,
                         dparams = list(),
                         line.p = c(.25, .75),
                         fullrange = FALSE,
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
    params = list2(
      distribution = distribution,
      dparams = dparams,
      na.rm = na.rm,
      line.p = line.p,
      fullrange = fullrange,
      ...
    )
  )
}

#' @export
#' @rdname geom_qq
stat_qq_line <- geom_qq_line

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQqLine <- ggproto("StatQqLine", Stat,
  default_aes = aes(x = after_stat(x), y = after_stat(y)),

  required_aes = c("sample"),

  dropped_aes = c("sample"),

  compute_group = function(data,
                           scales,
                           quantiles = NULL,
                           distribution = stats::qnorm,
                           dparams = list(),
                           na.rm = FALSE,
                           line.p = c(.25, .75),
                           fullrange = FALSE) {

    sample <- sort(data$sample)
    n <- length(sample)

    # Compute theoretical quantiles
    if (is.null(quantiles)) {
      quantiles <- stats::ppoints(n)
    } else if (length(quantiles) != n) {
      cli::cli_abort("{.arg quantiles} must have the same length as the data.")
    }

    theoretical <- inject(distribution(p = quantiles, !!!dparams))

    if (length(line.p) != 2) {
      cli::cli_abort("Cannot fit line quantiles {line.p}. {.arg line.p} must have length 2.")
    }

    x_coords <- inject(distribution(p = line.p, !!!dparams))
    y_coords <- stats::quantile(sample, line.p)
    slope <- diff(y_coords) / diff(x_coords)
    intercept <- y_coords[1L] - slope * x_coords[1L]

    if (fullrange & !is.null(scales$x$dimension)) {
      x <- scales$x$dimension()
    } else {
      x <- range(theoretical)
    }

    data_frame0(x = x, y = slope * x + intercept)
  }
)
