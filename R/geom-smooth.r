GeomSmooth <- proto(GeomInterval, {
  draw <- function(., data, scales, coordinates, ...) {
    data$fill <- alpha(data$fill, data$alpha)
    gList(
      tryNULL(GeomRibbon$draw(transform(data, colour=NA), scales, coordinates)),
      GeomPath$draw(rename(data, c(middle = "y")), scales, coordinates)
    )
  }

  objname <- "smooth"
  desc <- "Add a smoothed condition mean."
  icon <- function(.) {
    gTree(children=gList(
      polygonGrob(c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0), c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7), gp=gpar(fill="grey60", col=NA)),
      linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.6, 0.4, 0.5, 0.4, 0.6))
    ))
  }
  
  guide_geom <- function(.) "smooth"
  
  default_stat <- function(.) StatSmooth
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="#3366FF", fill="black", size=1, linetype=1, weight=1, alpha=0.2)


  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    data$fill <- alpha(data$fill, data$alpha)
    
    gTree(children = gList(
      GeomTile$draw_legend(data, ...),
      GeomPath$draw_legend(data, ...)
    ))
  }
  examples <- function(.) {
    # See stat_smooth for examples of using built in model fitting
    # if you need some more flexible, this example shows you how to
    # plot the fits from any model of your choosing
    
    library(ggplot2)
    qplot(wt, mpg, data=mtcars, colour=factor(cyl))

    model <- lm(mpg ~ wt + factor(cyl), data=mtcars)
    grid <- with(mtcars, expand.grid(
      wt = seq(min(wt), max(wt), length = 20),
      cyl = levels(factor(cyl))
    ))

    grid$mpg <- predict(model, newdata=grid)

    qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + geom_line(data=grid)

    # or with standard errors

    err <- predict(model, newdata=grid, se = TRUE)
    grid$ucl <- err$fit + 1.96 * err$se.fit
    grid$lcl <- err$fit - 1.96 * err$se.fit

    qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + geom_smooth(aes(min=lcl, max=ucl), data=grid, stat="identity") 
  }

})
