PositionFill <- proto(Position, {
  adjust <- function(., data, scales) {
    if (is.null(data)) return() 
    
    y <- scales$get_scales("y")
    y$limits <- c(0, 1)
    
    check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
    if (!all(data$ymin == 0)) warning("Filling not well defined when ymin != 0")
    collide(data, .$width, .$my_name(), pos_fill)
  }  

  objname <- "fill"
  desc <- "Stack overlapping objects on top of one another, and standardise have equal height"

  icon <- function(.) {
    y <- c(0.5, 0.8)
    rectGrob(0.5, c(0.625, 1), width=0.4, height=c(0.625, 0.375), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }


  examples <- function(.) {
    # See ?geom_bar and ?geom_area for more examples
    ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) + geom_bar(position="fill")
      
    cde <- geom_histogram(position="fill", binwidth = 500)
      
    ggplot(diamonds, aes(x=price)) + cde
    ggplot(diamonds, aes(x=price, fill=cut)) + cde
    ggplot(diamonds, aes(x=price, fill=clarity)) + cde
    ggplot(diamonds, aes(x=price, fill=color)) + cde
  }


})


