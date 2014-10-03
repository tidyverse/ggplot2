#' Adjust position by jittering inside the data range.
#'
#' This produces nicer plots in some situations, especially for
#' logistic regression plots. The default values for \code{width} and
#' \code{height} have been tailored for plots of binary data against a
#' predictor. The width defaults to zero, while the height default to
#' 5\% of the data resolution (0.05 in case of a 0-1 binary variable).
#'
#' @family position adjustments
#' @param width degree of jitter in x direction. Defaults to 5\% of
#'   the resolution of the data.
#' @param height degree of jitter in y direction. Defaults to 0.
#' @param bound scales whose ranges bound the jitter. Can be
#'   \code{"x"}, \code{"y"} or \code{c("x", "y")}. Defaults to
#'   \code{"y"}.
#' @export
#' @examples
#' movies_sub <- movies[sample(nrow(movies), 1000), ]
#' ggplot(movies_sub, aes(year, Comedy)) +
#'   geom_point(position = position_jitterin(), alpha = 0.5) +
#'   stat_smooth(method = "glm", family = "binomial", level = 0)
position_jitterin <- function (width = NULL, height = NULL, bound = "y") {
  PositionJitterin$new(width = width, height = height, bound = bound)
}

PositionJitterin <- proto(Position, {
  width <- NULL
  height <- NULL
  bound <- NULL

  new <- function(., width = NULL, height = NULL, bound = "y") {
    .$proto(width = width, height = height, bound = bound)
  }

  objname <- "position_jitterin"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_jitterin")

    if (is.null(.$width)) {
      .$width <- 0
    }
    if (is.null(.$height)) {
      .$height <- resolution(data$y, zero = FALSE) * 0.05
    }

    bound_x <- "x" %in% .$bound
    bound_y <- "y" %in% .$bound
    if ((bound_x && !length(unique(data$x)) == 2) ||
          (bound_y && !length(unique(data$y)) == 2))
      stop("position_jitterin can only bound binary scales", call. = FALSE)

    trans_x <- NULL
    trans_y <- NULL
    if(.$width > 0) {
      trans_x <- function(x) {
        jitter <- if (bound_x) jitterin else base::jitter
        jitter(x, amount = .$width)
      }
    }
    if(.$height > 0) {
      trans_y <- function(x) {
        jitter <- if (bound_y) jitterin else base::jitter
        jitter(x, amount = .$height)
      }
    }

    transform_position(data, trans_x, trans_y)
  }

})

jitterin <- function(x, amount) {
  u <- stats::runif(length(x), 0, amount)
  range <- range(x, na.rm = TRUE)
  ifelse(x == range[1], range[1] + u, range[2] - u)
}
