#' Add a smoother.
#' 
#' Aids the eye in seeing patterns in the presence of overplotting.
#'
#' Calculation is performed by the (currently undocumented) 
#' \code{predictdf} generic function and its methods.  For most methods
#' the confidence bounds are computed using the \code{\link{predict}}
#' method - the exceptions are \code{loess} which uses a t-based 
#' approximation, and for \code{glm} where the normal confidence interval
#' is constructed on the link scale, and then back-transformed to the response
#' scale.
#'
#' @param method smoothing method (function) to use, eg. lm, glm, gam, loess,
#'   rlm
#' @param formula formula to use in smoothing function, eg. \code{y ~ x}, 
#'   \code{y ~ poly(x, 2)}, \code{y ~ log(x)}
#' @param se display confidence interval around smooth? (TRUE by default, see
#'   level to control
#' @param fullrange should the fit span the full range of the plot, or just
#'   the data
#' @param level level of confidence interval to use (0.95 by default)
#' @param n number of points to evaluate smoother at
#' @param xseq exact points to evaluate smooth at, overrides n
#' @param ... other arguments are passed to smoothing function
#' @inheritParams stat_identity
#' @return a data.frame with additional columns 
#'   \item{y}{predicted value}
#'   \item{ymin}{lower pointwise confidence interval around the mean}
#'   \item{ymax}{upper pointwise confidence interval around the mean}
#'   \item{se}{standard error}
#' @seealso  
#'   \code{\link{lm}} for linear smooths,
#'   \code{\link{glm}} for generalised linear smooths,
#'   \code{\link{loess}} for local smooths
#' @export
#' @examples
#' c <- ggplot(mtcars, aes(qsec, wt))
#' c + stat_smooth() 
#' c + stat_smooth() + geom_point()
#' 
#' # Adjust parameters
#' c + stat_smooth(se = FALSE) + geom_point()
#' 
#' c + stat_smooth(span = 0.9) + geom_point()  
#' c + stat_smooth(level = 0.99) + geom_point()
#' c + stat_smooth(method = "lm") + geom_point() 
#' 
#' library(splines)
#' library(MASS)
#' c + stat_smooth(method = "lm", formula = y ~ ns(x,3)) +
#'   geom_point()  
#' c + stat_smooth(method = rlm, formula= y ~ ns(x,3)) + geom_point()  
#' 
#' # The default confidence band uses a transparent colour. 
#' # This currently only works on a limited number of graphics devices 
#' # (including Quartz, PDF, and Cairo) so you may need to set the
#' # fill colour to a opaque colour, as shown below
#' c + stat_smooth(fill = "grey50", size = 2, alpha = 1)
#' c + stat_smooth(fill = "blue", size = 2, alpha = 1)
#' 
#' # The colour of the line can be controlled with the colour aesthetic
#' c + stat_smooth(fill="blue", colour="darkblue", size=2)
#' c + stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2)
#' c + geom_point() + 
#'   stat_smooth(fill="blue", colour="darkblue", size=2, alpha = 0.2)
#' 
#' # Smoothers for subsets
#' c <- ggplot(mtcars, aes(y=wt, x=mpg)) + facet_grid(. ~ cyl)
#' c + stat_smooth(method=lm) + geom_point() 
#' c + stat_smooth(method=lm, fullrange=T) + geom_point() 
#' 
#' # Geoms and stats are automatically split by aesthetics that are factors
#' c <- ggplot(mtcars, aes(y=wt, x=mpg, colour=factor(cyl)))
#' c + stat_smooth(method=lm) + geom_point() 
#' c + stat_smooth(method=lm, aes(fill = factor(cyl))) + geom_point() 
#' c + stat_smooth(method=lm, fullrange=TRUE, alpha = 0.1) + geom_point() 
#' 
#' # Use qplot instead
#' qplot(qsec, wt, data=mtcars, geom=c("smooth", "point"))
#' 
#' # Example with logistic regression
#' data("kyphosis", package="rpart")
#' qplot(Age, Kyphosis, data=kyphosis)
#' qplot(Age, data=kyphosis, facets = . ~ Kyphosis, binwidth = 10)
#' qplot(Age, Kyphosis, data=kyphosis, position="jitter")
#' qplot(Age, Kyphosis, data=kyphosis, position=position_jitter(height=0.1))
#' 
#' qplot(Age, as.numeric(Kyphosis) - 1, data = kyphosis) +
#'   stat_smooth(method="glm", family="binomial")
#' qplot(Age, as.numeric(Kyphosis) - 1, data=kyphosis) +
#'   stat_smooth(method="glm", family="binomial", formula = y ~ ns(x, 2))
stat_smooth <- function (mapping = NULL, data = NULL, geom = "smooth", position = "identity", 
method = "auto", formula = y ~ x, se = TRUE, n = 80, fullrange = FALSE, 
level = 0.95, na.rm = FALSE, ...) { 
  StatSmooth$new(mapping = mapping, data = data, geom = geom, position = position, 
  method = method, formula = formula, se = se, n = n, fullrange = fullrange, 
  level = level, na.rm = na.rm, ...)
}

StatSmooth <- proto(Stat, {
  objname <- "smooth"

  calculate_groups <- function(., data, scales, ...) {
    rows <- daply(data, .(group), function(df) length(unique(df$x)))
    
    if (all(rows == 1) && length(rows) > 1) {
      message("geom_smooth: Only one unique x value each group.", 
        "Maybe you want aes(group = 1)?")
      return(data.frame())
    }
    
    .super$calculate_groups(., data, scales, ...)
  }
  
  calculate <- function(., data, scales, method="auto", formula=y~x, se = TRUE, n=80, fullrange=FALSE, xseq = NULL, level=0.95, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_smooth")
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }
    
    # Figure out what type of smoothing to do: loess for small datasets,
    # gam with a cubic regression basis for large data
    if (is.character(method) && method == "auto") {
      if (nrow(data) < 1000) {
        method <- "loess"
      } else {
        try_require("mgcv")
        method <- gam
        formula <- y ~ s(x, bs = "cs")
      }
    }
    
    if (is.null(data$weight)) data$weight <- 1
    
    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scale_dimension(scales$x, c(0, 0)) 
        } else {
          xseq <- sort(unique(data$x))
        } 
      } else {
        if (fullrange) {
          range <- scale_dimension(scales$x, c(0, 0))
        } else {
          range <- range(data$x, na.rm=TRUE)  
        } 
        xseq <- seq(range[1], range[2], length=n)
      } 
    }
    if (is.character(method)) method <- match.fun(method)
    
    method.special <- function(...) 
      method(formula, data=data, weights=weight, ...)
    model <- safe.call(method.special, list(...), names(formals(method)))
    
    predictdf(model, xseq, se, level)
  }
  
  icon <- function(.) GeomSmooth$icon()
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomSmooth
})
