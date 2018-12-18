#' Compute data/confidence ellipses for a bivariate dataset
#'
#' The method for calculating the ellipses has been modified from
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
#'   `"confidence" draws a confidence ellipse for a given regression model.
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
#'   stat_ellipse(type = "conf", color = "red", size = 2)
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#'   geom_point() +
#'   stat_ellipse(type = "norm", linetype = 2) +
#'   stat_ellipse(type = "euclid", level = 3) +
#'   coord_fixed()
#'
#' ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#'   stat_ellipse(geom = "polygon")
#'
#' TO DO: Still at the debugging stage: confidence ellipse doesn't look right!
#' TO DO: add a method argument, e.g. method = "lm", method = "glm", etc.
#' TO DO: allow partial matching so 'type = "normal"' and 'type = "conf"' work
#' TO DO: allow formula to be passed as string "y ~ x" 
#' TO DO: make sure methods "t" and "norm" can find "x" and "y" in multivariate dataset
#' TO DO: allow overruling of degrees of freedom with options dfn and dfd 
#' TO DO: check that option 'segments' works 
#' TO DO: should I use stats::qf() or simply qf() ?
#' TO DO: pass fill and alpha to the ellipse constructor
#' TO DO: Why not use center <- colMeans(data[,vars]) ?
#' TO DO: allow overruling of center. 
#' TO DO: add prediction ellipses as well as confidence ellipses!
#' 
#' 
stat_ellipse <- function(data = NULL, 
                         mapping = NULL, 
                         formula = NULL, 
                         geom = "path", 
                         position = "identity",
                         ...,
                         type = "t",
                         level = 0.95,
                         segments = 51,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

is.formula <- function(x) inherits(x, "formula") 
if (!is.formula(formula)) stop("argument 'formula' for confidence ellipses must be passed as a formula, e.g. 'formula = y ~ x' without quote marks. If type is set to 'confidence' The function takes the formula and applies the 'linear model' function 'lm(formula)' (ordinary least squares). It cannot currently handle other models. When passing multivariate models, the variable of interest must be passed first, e.g. if 'y ~ x + z', the variables 'y' and 'x' will be used to compute the ellipse.  Degrees of freedom are computed from the model and cannot be overruled.", call. = FALSE)

  layer(
    data = data,
    mapping = mapping,
    stat = StatEllipse,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      formula = formula, 
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
  compute_group = function(data, formula = y ~ x, scales, type = "t", level = 0.95,
                           segments = 51, na.rm = FALSE) {
    calculate_ellipse(data = data, vars = c("x", "y"), formula = formula, type = type,
                      level = level, segments = segments)
  }
)

calculate_ellipse <- function(data, vars, formula, type, level, segments){
  
  # accept only recognized ellipse types
  if (!type %in% c("t", "norm", "euclid", "confidence")) {
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

  # compute ellipse for each type 
  # start with "confidence" ellipse 
  if (type == "confidence") {
    print("Just entered 'type == confidence':")  # remove after debugging
    # compute center of ellipse
    #center <- colMeans(data[,vars])
    center <- MASS::cov.trob(data[,vars])$center  # car uses this, why not colMeans?
    # fit a linear model
    fit <- lm(formula, data = data) 
    # compute degrees of freedom
    dfn <- length(coef(fit))
    dfd <- df.residual(fit)
    # compute variance/covariance matrix
    vcov <- vcov(fit)
    # extract appropriate coefficients from model 
      # only handles linear model with intercept
      # x variable must be first passed
    which.coef <- c(1, 2) 
    shape <- vcov[which.coef, which.coef] 
    # compute rotation of ellipse via Cholesky decomposition
    # code from "car" package, version 3.0-2 (18 December 2018)
    Q <- chol(shape, pivot = TRUE) 
    order <- order(attr(Q, "pivot"))
    rotation <- t(unit.circle %*% Q[, order]) 
    # older versions of the "car" package used:
      # Q <- chol(shape, pivot = FALSE)
      # rotation <- t(unit.circle %*% Q) 
    # compute radius of ellipse 
    radius <- sqrt(dfn * stats::qf(level, dfn, dfd))
    # center/radius/rotation variables have now been computed for type "confidence"
  # compute ellipse for types other than "confidence"
  } else {
    # compute degrees of freedom for types "norm", "t", and "euclid"
    dfn <- 2
    dfd <- nrow(data) - 1
    # compute variance/covariance matrix 
    if (type == "t") { 
      v <- MASS::cov.trob(data[,vars])
    } else if (type == "norm") {
      v <- stats::cov.wt(data[,vars])
    } else if (type == "euclid") {
      v <- stats::cov.wt(data[,vars]) 
      v$cov <- diag(rep(min(diag(v$cov)), 2))
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
  }
  # center/radius/rotation variables have now been computed for all types
  # compute the ellipse from the center/radius/rotation variables
  ellipse <- t(center + radius * rotation)  
  colnames(ellipse) <- vars 
  mat_2_df(ellipse) 
  
  # show data in console for debugging purposes
  ellipse <- mat_2_df(ellipse) 
  message("Debug: sorry for filling your console with data!")
  print(ellipse) 
  return(ellipse)

}
