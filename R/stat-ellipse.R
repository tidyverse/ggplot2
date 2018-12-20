#' Compute data ellipses for a bivariate dataset
#'
#' The method for calculating the ellipses is adapted from
#' `car::ellipse` (https://CRAN.R-project.org/package=car )
#'
#' @references John Fox and Sanford Weisberg (2019). An {R} Companion to
#'   Applied Regression, Third Edition. Thousand Oaks CA: Sage. URL:
#'   \url{http://socserv.socsci.mcmaster.ca/jfox/Books/Companion}
#' @param level The confidence level at which to draw an ellipse (default is 0.95),
#'   or, if `type="euclid"`, the radius of the circle to be drawn.
#' @param type The type of ellipse.
#'  The default `"norm"` draw a data ellipse for a bivariate normal distribution.
#'   `"norm"` assumes a bivariate normal distribution.
#'   `"t"` assumes a bivariate t-distribution.
#'   `"euclid"` draws a circle with the radius equal to `level`.
#'   representing the euclidean distance from the center.
#'   This ellipse probably won't appear circular unless `coord_fixed()` is applied.
#' @param segments The number of segments to be used in drawing the ellipse.
#' @inheritParams layer
#' @inheritParams geom_point
#' @export
#' @examples
#' ggplot(faithful, aes(waiting, eruptions)) +
#'   geom_point() +
#'   stat_ellipse()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#'   geom_point() +
#'   stat_ellipse()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#'   geom_point() +
#'   stat_ellipse(type = "norm", linetype = 2) +
#'   stat_ellipse(type = "t")
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#'   geom_point() +
#'   stat_ellipse(type = "norm", linetype = 2) +
#'   stat_ellipse(type = "euclid", level = 3) +
#'   coord_fixed()
#'
#' ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#'   stat_ellipse(geom = "polygon")
stat_ellipse <- function(mapping = NULL, data = NULL,
                         geom = "path", position = "identity",
                         ...,
                         type = "t",
                         level = 0.95,
                         segments = 51,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEllipse,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      level = level,
      segments = segments,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatEllipse <- ggproto("StatEllipse", Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, type = "t", level = 0.95,
                           segments = 51, na.rm = FALSE) {
    calculate_ellipse(data = data, vars = c("x", "y"), type = type,
                      level = level, segments = segments)
  }
)

calculate_ellipse <- function(data, vars, type, level, segments){

  # accept only recognized ellipse types
  if (!type %in% c("t", "norm", "euclid")) {
    message("Unrecognized ellipse type")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  # stop if dataset is too small
  } else if (nrow(data) < 4) {
    message("Too few points to calculate an ellipse")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } 
  
  # compute a unit circle as basis for ellipse (used by all types)
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))
  
  # compute degrees of freedom for types "norm", "t", and "euclid"
  dfn <- 2
  dfd <- nrow(data) - 1

  # compute variance/covariance matrix 
  if (type == "euclid") {
    v <- stats::cov.wt(data[,vars]) 
    v$cov <- diag(rep(min(diag(v$cov)), 2))
  } else if (type == "t") { 
    v <- MASS::cov.trob(data[,vars])
  } else  { # type == "norm" is the third and last option
    v <- stats::cov.wt(data[,vars])
  }

  # compute center of ellipse
  center <- v$center

  # compute rotation of ellipse via Cholesky decomposition
  shape <- v$cov
  Q <- chol(shape, pivot = TRUE) 
  order <- order(attr(Q, "pivot"))
  rotation <- t(unit.circle %*% Q[, order]) 
  # compute radius of ellipse
  if (type == "euclid") {
    radius <- level/max(Q) 
  } else {
    radius <- sqrt(dfn * stats::qf(level, dfn, dfd)) 
  }

  # compute the ellipse from the center/radius/rotation variables
  ellipse <- t(center + radius * rotation)  
  colnames(ellipse) <- vars 
  mat_2_df(ellipse) 
}
