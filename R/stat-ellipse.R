#' Plot data ellipses.
#'
#' @param level The confidence level at which to draw an ellipse (default is 0.95),
#'   or, if \code{type="euclid"}, the radius of the circle to be drawn.
#' @param type The type of ellipse.
#'   The default \code{"t"} assumes a multivariate t-distribution, and
#'   \code{"norm"} assumes a multivariate normal distribution.
#'   \code{"euclid"} draws a circle with the radius equal to \code{level},
#'   representing the euclidian distance from the center.
#'   This ellipse probably won't appear circular unless \code{coord_fixed()} is applied.
#' @param segments The number of segments to be used in drawing the ellipse.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'   a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams stat_identity
#'
#' @details The method for calculating the ellipses has been modified from car::ellipse (Fox and Weisberg, 2011)
#'
#' @references
#' John Fox and Sanford Weisberg (2011). An {R} Companion to Applied Regression, Second Edition. Thousand Oaks CA: Sage. URL: http://socserv.socsci.mcmaster.ca/jfox/Books/Companion
#'
#' @export
#' @importFrom MASS cov.trob
#'
#' @examples
#' ggplot(faithful, aes(waiting, eruptions))+
#'   geom_point()+
#'   stat_ellipse()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'   geom_point()+
#'   stat_ellipse()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'   geom_point()+
#'   stat_ellipse(type = "norm", linetype = 2)+
#'   stat_ellipse(type = "t")
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'   geom_point()+
#'   stat_ellipse(type = "norm", linetype = 2)+
#'   stat_ellipse(type = "euclid", level = 3)+
#'   coord_fixed()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'   stat_ellipse(geom = "polygon")

stat_ellipse <- function(mapping = NULL, data = NULL, geom = "path", position = "identity", type = "t", level = 0.95, segments = 51, na.rm = FALSE, ...) {
  StatEllipse$new(mapping = mapping, data = data, geom = geom, position = position, type = type, level = level, segments = segments, na.rm = na.rm, ...)
}

StatEllipse <- proto(Stat, {
  objname <- "ellipse"

  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPath

  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  calculate <- function(., data, scales, type = "t", level = 0.95, segments = 51, na.rm = FALSE, ...){
    data <- remove_missing(data, na.rm, vars = c("x","y"), name = "stat_ellipse", finite = TRUE)
    ellipse <- calculate_ellipse(data=data, vars= c("x","y"), type=type, level=level, segments=segments)
    return(ellipse)
  }
})

calculate_ellipse <- function(data, vars, type, level, segments){
  dfn <- 2
  dfd <- nrow(data) - 1

  if (!type %in% c("t", "norm", "euclid")){
    message("Unrecognized ellipse type")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else if (dfd < 3){
    message("Too few points to calculate an ellipse")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } else {
    if (type == "t"){
      v <- cov.trob(data[,vars])
    } else if (type == "norm"){
      v <- cov.wt(data[,vars])
    } else if (type == "euclid"){
      v <- cov.wt(data[,vars])
      v$cov <- diag(rep(min(diag(v$cov)), 2))
    }
    shape <- v$cov
    center <- v$center
    chol_decomp <- chol(shape)
    if (type == "euclid"){
      radius <- level/max(chol_decomp)
    } else {
      radius <- sqrt(dfn * qf(level, dfn, dfd))
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  }

  ellipse <- as.data.frame(ellipse)
  colnames(ellipse) <- vars
  return(ellipse)
}
