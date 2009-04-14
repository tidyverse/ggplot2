PositionStack <- proto(Position, {

  adjust <- function(., data, scales) {
    if (empty(data)) return(data.frame())
    
    if (is.null(data$ymax)) {
      message("Missing ymax in position = 'stack'. ", 
        "Maybe you want position = 'identity'?")
      return(data)
    }

    if (!all(data$ymin == 0)) 
      warning("Stacking not well defined when ymin != 0")
    collide(data, .$width, .$my_name(), pos_stack)
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
      
    ggplot(diamonds, aes(x=price)) + geom_histogram(binwidth=500)
    ggplot(diamonds, aes(x=price, fill=cut)) + geom_histogram(binwidth=500)
  }
})