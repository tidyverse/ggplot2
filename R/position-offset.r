position_offset <- function (width = NULL, height = NULL) { 
  PositionOffset$new(width = width, height = height)
}

PositionOffset <- proto(Position, {
  objname <- "offset"
 
  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_offset")
    
    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$height)) .$height <- resolution(data$y, zero = FALSE) * 0.4
    
    trans_x <- function(x) x + .$width
    trans_y <- function(x) x + .$height
    
    transform_position(data, trans_x, trans_y)
  }
})
