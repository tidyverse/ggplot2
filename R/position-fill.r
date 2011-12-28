#' Stack overlapping objects on top of one another, and standardise to have
#' equal height.
#' 
#' @export
#' @examples
#' # See ?geom_bar and ?geom_area for more examples
#' ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) +
#'   geom_bar(position="fill")
#'   
#' cde <- geom_histogram(position="fill", binwidth = 500)
#'   
#' ggplot(diamonds, aes(x=price)) + cde
#' ggplot(diamonds, aes(x=price, fill=cut)) + cde
#' ggplot(diamonds, aes(x=price, fill=clarity)) + cde
#' ggplot(diamonds, aes(x=price, fill=color)) + cde
position_fill <- function (width = NULL, height = NULL) { 
  PositionFill$new(width = width, height = height)
}
  
PositionFill <- proto(Position, {
  objname <- "fill"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    
    check_required_aesthetics(c("x", "ymax"), names(data), "position_fill")
    if (!all(data$ymin == 0)) warning("Filling not well defined when ymin != 0")
    collide(data, .$width, .$my_name(), pos_fill)
  }  

  icon <- function(.) {
    y <- c(0.5, 0.8)
    rectGrob(0.5, c(0.625, 1), width=0.4, height=c(0.625, 0.375), gp=gpar(col="grey60", fill=c("#804070", "#668040")), vjust=1)
  }
})
