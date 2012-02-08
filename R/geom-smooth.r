#' Add a smoothed conditional mean.
#'
#' @inheritParams geom_point
#' @seealso The default stat for this geom is \code{\link{stat_smooth}} see 
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @examples
#' # See stat_smooth for examples of using built in model fitting
#' # if you need some more flexible, this example shows you how to
#' # plot the fits from any model of your choosing
#' qplot(wt, mpg, data=mtcars, colour=factor(cyl))
#' 
#' model <- lm(mpg ~ wt + factor(cyl), data=mtcars)
#' grid <- with(mtcars, expand.grid(
#'   wt = seq(min(wt), max(wt), length = 20),
#'   cyl = levels(factor(cyl))
#' ))
#' 
#' grid$mpg <- stats::predict(model, newdata=grid)
#' 
#' qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + geom_line(data=grid)
#' 
#' # or with standard errors
#' 
#' err <- stats::predict(model, newdata=grid, se = TRUE)
#' grid$ucl <- err$fit + 1.96 * err$se.fit
#' grid$lcl <- err$fit - 1.96 * err$se.fit
#' 
#' qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + 
#'   geom_smooth(aes(ymin = lcl, ymax = ucl), data=grid, stat="identity") 
geom_smooth <- function (mapping = NULL, data = NULL, stat = "smooth", position = "identity", ...) { 
  GeomSmooth$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomSmooth <- proto(Geom, {
  objname <- "smooth"

  draw <- function(., data, scales, coordinates, ...) {
    ribbon <- transform(data, colour = NA)
    path <- transform(data, alpha = 1)
    
    has_ribbon <- function(x) !is.null(data$ymax) && !is.null(data$ymin)
        
    gList(
      if (has_ribbon(data)) GeomRibbon$draw(ribbon, scales, coordinates),
      GeomLine$draw(path, scales, coordinates)
    )
  }

  icon <- function(.) {
    gTree(children=gList(
      polygonGrob(c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0), c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7), gp=gpar(fill="grey60", col=NA)),
      linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.6, 0.4, 0.5, 0.4, 0.6))
    ))
  }
  
  guide_geom <- function(.) "smooth"
  
  default_stat <- function(.) StatSmooth
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="#3366FF", fill="grey60", size=0.5, linetype=1, weight=1, alpha=0.4)


  draw_legend <- function(., data, params, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    data$fill <- alpha(data$fill, data$alpha)
    data$alpha <- 1
    
    if (is.null(params$se) || params$se) {
      gTree(children = gList(
        rectGrob(gp = gpar(col = NA, fill = data$fill)),
        GeomPath$draw_legend(data, ...)
      ))      
    } else {
      GeomPath$draw_legend(data, ...)
    }
  }

})
