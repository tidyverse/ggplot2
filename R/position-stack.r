PositionStack <- proto(Position, {
  rescale <- FALSE

  adjust <- function(., data, scales) {
    if (is.null(data)) return() 
    
    check_required_aesthetics(c("x", "y"), names(data), "position_stack")
    if (!all(data$ymin == 0)) warning("Stacking not well defined when ymin != 0")
  
    adjust <- function(data) {
      data <- data[order(data$x), ]
      
      y <- with(data, ifelse(is.na(y), 0, y))
      heights <- c(0, cumsum(y))
      if (.$rescale) heights <- rescale(heights, c(0,1))
      transform(data, 
        ymin = heights[-length(heights)],
        ymax = heights[-1],
        y = heights[-1]
      )
    }
    
    xs <- split(data, data$x)
    data <- do.call("rbind", lapply(xs, adjust))
  
    data
  }
  
  objname <- "stack"
  desc <- "Stack overlapping objects on top of one another"
  icon <- function(.) {
    y <- c(0.5, 0.8)
    rectGrob(0.5, c(0.5, 0.8), width=0.4, height=c(0.5, 0.3), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }
  examples <- function(.) {
    # See ?geom_bar and ?geom_area for more examples
    ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) + geom_bar()
      
    ggplot(diamonds, aes(x=price)) + geom_bar()
    ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar()
  }
})

PositionFill <- proto(PositionStack, {
  rescale <- TRUE
  objname = "fill"
  desc <- "Stack overlapping objects on top of one another, and standardise have equal height"

  icon <- function(.) {
    y <- c(0.5, 0.8)
    rectGrob(0.5, c(0.625, 1), width=0.4, height=c(0.625, 0.375), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }


  examples <- function(.) {
    # See ?geom_bar and ?geom_area for more examples
    ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) + geom_bar(position="fill")
      
    ggplot(diamonds, aes(x=price)) + geom_bar(position="fill")
    ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar(position="fill")
    ggplot(diamonds, aes(x=price, fill=clarity)) + geom_bar(position="fill")
    ggplot(diamonds, aes(x=price, fill=color)) + geom_bar(position="fill")
  }


})