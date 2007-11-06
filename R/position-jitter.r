PositionJitter <- proto(Position, {
  
  xjitter <- NULL
  yjitter <- NULL
  new <- function(., xjitter=NULL, yjitter=NULL) {
    .$proto(xjitter=xjitter, yjitter=yjitter)
  }
  
  adjust <- function(., data, scales) {
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")
    
    xrange <- diff(scales$get_scales("x")$frange())
    yrange <- diff(scales$get_scales("y")$frange())
    
    if (is.null(.$xjitter)) .$xjitter <- (is.integeric(resolution(data$x))) * 1
    if (is.null(.$yjitter)) .$yjitter <- (is.integeric(resolution(data$y))) * 1
    
    data <- transform(data, 
      x = jitter(x, amount = .$xjitter * xrange/50),
      y = jitter(y, amount = .$yjitter * yrange/50)
    )

    suppressWarnings(scales$get_scales("x")$train(data$x))
    suppressWarnings(scales$get_scales("y")$train(data$y))
    data
  }
  
  position <- "after"
  objname <- "jitter" 
  desc <- "Jitter points to avoid overplotting"
  
  icon <- function(.) GeomJitter$icon()
  desc_params <- list(
    xjitter = "degree of jitter in x direction, see ?jitter for details, defaults to 1 if the x variable is a factor, 0 otherwise", 
    yjitter = "degree of jitter in y direction, see ?jitter for details, defaults to 1 if the y variable is a factor, 0 otherwise"
    )

  examples <- function(.) {
    qplot(am, vs, data=mtcars)
    qplot(am, vs, data=mtcars, position="jitter")
    # Control amount of jittering by calling position_jitter
    qplot(am, vs, data=mtcars, position=position_jitter(x=10, y=0))
    qplot(am, vs, data=mtcars, position=position_jitter(x=0.5, y=0.5))
    
    # See lots of actually useful examples at geom_jitter
    # You can, however, jitter any geom, however little sense it might make
    qplot(cut, clarity, data=diamonds, geom="blank", group=1) + geom_path()
    qplot(cut, clarity, data=diamonds, geom="blank", group=1) + geom_path(position="jitter")
  }
  
})
