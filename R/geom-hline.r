GeomHline <- proto(Geom, {
  new <- function(., mapping=aes(), data=NULL, intercept=0, ...) {
    if (missing(data)) {
      data <- data.frame(intercept = intercept)
    }
    mapping <- defaults(mapping, aes(intercept=intercept, x=NULL, y=NULL, group=1))
    class(mapping) <- "uneval"
    layer(mapping=mapping, data=data, geom = ., geom_params = list(...))
  }

  draw <- function(., data, scales, coordinates, ...) {
    xrange <- coordinates$frange()$x

    ggname(.$my_name(), gTree(children=do.call(gList, lapply(1:nrow(data), function(i) {
      row <- data[c(i, i), ]
      row$x <- xrange
      row$y <- row$intercept

      GeomPath$draw(row, scales, coordinates)
    }))))
  }

  objname <- "hline"
  desc <- "Line, horizontal"
  icon <- function(.) linesGrob(c(0, 1), c(0.5, 0.5))
  details <- "<p>This geom allows you to annotate the plot with horizontal lines (see geom_vline and geom_abline for other types of lines)</p>\n\n<p>There are two ways to use it.  You can either specify the intercept of the line in the call to the geom, in which case the line will be in the same position in every panel.  Alternatively, you can supply a different intercept for each panel using a data.frame.  See the examples for the differences</p>"
    
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) c(GeomPath$default_aes(), aes(intercept=0))
  
  seealso <- list(
    geom_vline = "for vertical lines",
    geom_abline = "for lines defined by a slope and intercept"
  )
  
  examples <- function(.) {
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()

    p + geom_hline(intercept=20)
    p + geom_hline(intercept=seq(10, 30, by=5))
    p + geom_hline(intercept=mean(mtcars$mpg), size=2)
  }  
})
