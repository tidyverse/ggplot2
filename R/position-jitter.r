PositionJitter <- proto(Position, {
  adjust <- function(., data, scales) {
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")
    
    if (is.null(.$width)) .$width <- resolution(data$x) * 0.40
    if (is.null(.$height)) .$height <- resolution(data$y) * 0.40
    
    trans_x <- NULL
    trans_y <- NULL
    if(.$width > 0) {
      trans_x <- function(x) jitter(x, amount = .$width)
    }
    if(.$height > 0) {
      trans_y <- function(x) jitter(x, amount = .$height)
    }
    
    transform_position(data, trans_x, trans_y)
  }
  
  objname <- "jitter" 
  desc <- "Jitter points to avoid overplotting"
  
  icon <- function(.) GeomJitter$icon()
  desc_params <- list(
    xjitter = "degree of jitter in x direction. Defaults to 40% of the resolution of the data.", 
    yjitter = "degree of jitter in y direction. Defaults to 40% of the resolution of the data."
    )

  examples <- function(.) {
    qplot(am, vs, data=mtcars)
    qplot(am, vs, data=mtcars, position="jitter")
    # Control amount of jittering by calling position_jitter
    qplot(am, vs, data=mtcars, position=position_jitter(w=10, h=0))
    qplot(am, vs, data=mtcars, position=position_jitter(w=0.5, h=0.5))
  }
  
})
