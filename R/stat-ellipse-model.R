# TO DO LIST:
# set options for degrees of freedom, for known variance/covariance matrix 
# allow a vector of levels, e.g. level = c(0.95, 0.99)
# pass fill and alpha to the ellipse constructor
# add prediction ellipses as well as confidence ellipses 
#
#' Computes confidence ellipses for regression models
#'
#' The method for calculating confidence ellipses is based on 
#' `car::ellipse` (https://CRAN.R-project.org/package=car )
#'
#' @references John Fox and Sanford Weisberg (2019). An {R} Companion to
#'   Applied Regression, Third Edition. Thousand Oaks CA: Sage. URL:
#'   \url{http://socserv.socsci.mcmaster.ca/jfox/Books/Companion}
#'
#' Work in progress: incomplete
#' @param formula A formula to be used in the linear model, eg. `y ~ x`,
#'   `y ~ poly(x, 2)`, `y ~ log(x)`
#' @param vars A vector of two variables to be selected among the variables
#'   passed to the formula, e.g. `vars = c("x", "y")`
#' @param type The type of ellipse, one of `"means"`, `"confidence"`, `"prediction"`.
#' @param level The confidence level at which to draw an ellipse (default is 0.95).
#' @param method Regression method to use, accepts either a character vector,
#'   e.g. `"auto"`, `"lm"`, `"glm"`, `"gam"`, `"loess"` or a function, e.g.
#'   `MASS::rlm` or `mgcv::gam`, `base::lm`, or `base::loess`.
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by `method`.
#' @param distribution Distribution used, defaults to chi-square with degrees of freedom equal to the number of parameters passed vial the formula. 
#' @param distribution.args List of additional arguments passed on to the 
#'   distribution function defined by `distribution`.
#' @param center Center of the ellipse. Defaults to the means of the variables passed via the formula. Can be overwritten.  
#' @param radius Radius of the ellipse. Defaults to square-root of a chi-square distribution with degrees of freedom set to the number of parameters. Can be overwritten.  
#' @param shape Shape of the ellipse. Defaults to the variance/covariance matrix of the linear model.  
#' @param n The number of points at which to evaluate predictions. 
#' @param segments The number of segments used in drawing the ellipse.
#'
#' @return An ellipse based on parameters and distributional assumptions
#'  The default `"confidence"` draws a confidence ellipse for a regression model slope parameters. If only one regressor is provided, the intercept is used by default. If more than two regressors are provided, the first two are used by default. Specific parameters may be specified by setting the optional ``vars`` parameter to the associated variables, e.g. setting ``formula = y ~ x1 + x2 + x3`` with ``type = conf`` and  ``vars = c(x2, x3)`` will plot the ellipse in the ``(b2, b3)`` parameter space.  
#'   `"means"` draws an ellipse for a regression model response variable and regressor variables. Specific variables may be specified by setting the optional ``vars`` parameter, e.g. setting ``formula = y ~ x1 + x2 + x3`` with ``type = means`` and  ``vars = c(y, x3)`` will plot the ellipse in the ``(y, x3)`` parameter space.  
#' @section Computed variables:
#' \describe{
#'   \item{center}{center of the ellipse}
#'   \item{radius}{radius of the ellipse}
#'   \item{shape}{shape of the ellipse}
#'   \item{ellipse}{boundary of the ellipse}#'
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' ggplot(faithful, aes(waiting, eruptions)) +
#'   geom_point() +
#'   stat_ellipse_model()
#'
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
#'   geom_point() +
#'   stat_ellipse_model(formula = waiting ~ eruptions, type = "means")
#'
#' ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
#'   stat_ellipse_model(geom = "polygon")
#' 
# order of mapping, data, geom, position is conventional
# better to set the mapping before the data argument
stat_ellipse_model <- 
  function(mapping = NULL, 
           data = NULL,
           geom = "path", 
           position = "identity",
           ...,
           formula = y ~ x, 
           vars = NULL,
           type = "conf",
           level = 0.95, 
           method = "lm",
           method.args = list(),
           distribution = NULL,
           distribution.args = list(),
           center = NULL,
           radius = NULL,
           shape = NULL,
           shadows = TRUE,
           shadows.args = list(),
           n = 101,
           segments = 51,
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
# order of data, mapping, stat, geom, position is conventional
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEllipseModel,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      formula = formula, 
      vars = vars,
      type = type,
      level = level,
      method = method,
      method.args = method.args,
      distribution = distribution,
      distribution.args = distribution.args,
      center = center,
      radius = radius,
      shape = shape,
      shadows = shadows,
      shadows.args = shadows.args,
      n = n,
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
StatEllipseModel <- 
  ggplot2::ggproto("StatEllipseModel", ggplot2::Stat,

    required_aes = c("x", "y"),

    compute_group = function(data, scales, formula = y ~ x, vars = NULL, type = "means", level = 0.95, method = "lm", method.args = list(), distribution = NULL, distribution.args = list(), center = NULL, radius = NULL, shape = NULL, shadows = TRUE, shadows.args = list(), n = 101, segments = 51, na.rm = FALSE) {
    
      calculate_ellipse(data = data, formula = formula, vars = vars, type = type, level = level, method = method, method.args = method.args, distribution = distribution, distribution.args = distribution.args, center = center, radius = radius, shape = shape, shadows = shadows, shadows.args = shadows.args, n = n, segments = segments)
  
  }
)

calculate_ellipse <- function(data, formula, vars, type, level, method, method.args, distribution, distribution.args, center, radius, shape, n, segments) {

#  if (!is.formula(formula)) stop("argument `formula` must be passed as a formula, e.g. `formula = y ~ x` without quote marks. The `stat_ellipse_model` function takes the formula and applies the `linear model` function `lm(model)` (ordinary least squares). It cannot currently handle other models. When passing multivariate models, the variables of interest can be passed first, e.g. if `y ~ x1 + x2 + x3`, the variables `x1` and `x2` will be used to compute the confidence ellipse for parameters `b1` and `b2`. The parameters of interest may also be stated explicitly via `vars = c(x1, x2)`.", call. = FALSE) 

  ## DEBUG
  ## data = mtcars; formula = y ~ x; vars = NULL; type = "conf"; level = 0.95; method = "lm"; method.args = list(); distribution = NULL; distribution.args = list(); center = NULL; radius = NULL; shape = NULL; xseq = NULL; n = 101; segments = 51; na.rm = FALSE; data$x = data$mpg; data$y = data$disp;

  # set acceptable ellipse types
  type <- revalue(type, 
    c("conf" = "confidence", 
      "pred" = "prediction", 
      "mean" = "means")) 

  # set acceptable distribution types
  distribution <- revalue(distribution, 
    c("chisq" = "qchisq",
      "chi-square" = "qchisq",
      "f" = "qf",
      "F" = "qf"))

  # stop if dataset is too small
  if (nrow(data) < 4) {
    message("Too few points to calculate an ellipse")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  } 

  # accept only recognized ellipse types
  if (!type %in% c("means", "confidence", "prediction")) { 
    message("Unrecognized ellipse type")
    ellipse <- rbind(as.numeric(c(NA, NA)))
  }

  # accept distribution passed as character
  # if (is.character(distribution)) distribution <- match.fun(distribution) 
  if (is.character(distribution)) tryCatch({
    distribution <- match.fun(distribution)
  },
  error = function(cond) {
    message(paste("Unrecognized distribution:", distribution,"\n"))
    message("Make sure you have selected an appropriate distribution. The recognized distributions are `stats::qf` and `stats::qchisq`. If a valid function is passed, I will attempt to use it together with the arguments set in `distribution.args`.")
    message(cond)
  }) 

  # set the distribution
  if (is.null(distribution)) {
    distribution <- stats::qchisq 
  }

  # fit a linear model 
  if (method == "lm") {
    fit <- stats::lm(formula, data) 
  } else {
      stop("Method '", method, "' not yet implemented.")
  }

  # set scene  
  coefs <- if (!(is.null(vars))) vars else { 
      if (length(coef(fit)) == 2) c(1, 2) else {
        if (any(names(coef(fit)) == "(Intercept)")) c(2, 3) else c(1, 2)
      }
    } 
  # set variables, e.g. c("x", "y")
  if (is.null(vars)) {
    scene <- names(coef(fit)[coefs])
    # vars <- rev(all.vars(formula)[coefs])
    vars <- all.vars(formula)  # all variables in regression
  }

  # compute a unit circle as basis for ellipse 
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))

  # compute center of ellipse
  if (is.null(center)) center <- colMeans(data[,vars]) 

  # compute radius of ellipse 
  if (is.null(radius)) {
    # compute degrees of freedom
    dfn <- length(coef(fit))
    dfd <- df.residual(fit)
    # compute radius from distribution
    # overwrite args if provided
    # radius <- if "df" %in% names(distribution.args) sqrt(do.call(distribution, distribution.args)) else {
    #  sqrt(do.call(distribution, c(list(df = dfn, distribution.args))))
    #} 
    radius <- sqrt(stats::qchisq(level, df = dfn)) # known variance
    # radius <- sqrt(dfn * stats::qf(level, dfn, dfd))  # unknown variance
  } 

  # compute shape of ellipse
  if (is.null(shape)) {
    # compute variance/covariance matrix of the fitted model
    vcov <- vcov(fit)   
  }

  # select the shape coefficients for the selected ellipse 
  #shape <- vcov[coefs, coefs] 
  shape <- vcov[scene, scene] 

  # compute rotation of ellipse via Cholesky decomposition
  # code from "car" package, version 3.0-2 (18 December 2018)
  Q <- chol(shape, pivot = TRUE) 
  order <- order(attr(Q, "pivot"))
  rotation <- t(unit.circle %*% Q[, order]) 

  # center/radius/shape variables have now been computed
  # they will now be mapped onto the appropriate plane 
  # compute ellipse for each type 
  if (type == "means") {
    ellipse <- t(center + radius * rotation)  
  } else { 
    # switch aes from (x, y) to (bx, by) 
    # how do you switch aes?
    ellipse <- t(center + radius * rotation) 
  }

  colnames(ellipse) <- vars 
  ellipse <- mat_2_df(ellipse) 

  # override options for debugging 
  shadows.args <- list(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
 )

  # draw ellipse shadows
  if (shadows) {
    shadows <- new_data_frame(c(
      list(
        xmin = min(ellipse[, vars[1]]),
        xmax = max(ellipse[, vars[1]]),
        ymin = min(ellipse[, vars[2]]),
        ymax = max(ellipse[, vars[2]])
      ),
      shadows.args
    ))
  }

  default_aes = aes(colour = "red", fill = "white", size = 0.5, alpha = NA, linetype = "solid")

  # show data in console for debugging purposes
  # message("Debug: sorry for filling your console with data!")
  # print(ellipse) 
  return(ellipse)

}
