PositionJitter <- proto(Position, {
  
  xjitter <- NULL
  yjitter <- NULL
  new <- function(., xjitter=NULL, yjitter=NULL) {
    .$proto(xjitter=xjitter, yjitter=yjitter)
  }
  
  adjust <- function(., data, scales) {
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")
    
    if (is.null(.$xjitter)) .$xjitter <- resolution(data$x) * 0.40
    if (is.null(.$yjitter)) .$yjitter <- resolution(data$y) * 0.40

    data <- transform(data, 
      x = jitter(x, amount = .$xjitter),
      y = jitter(y, amount = .$yjitter)
    )
    
    data
  }
  
  position <- "after"
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
    qplot(am, vs, data=mtcars, position=position_jitter(x=10, y=0))
    qplot(am, vs, data=mtcars, position=position_jitter(x=0.5, y=0.5))
    
    # See lots of actually useful examples at geom_jitter
    # You can, however, jitter any geom, however little sense it might make
    qplot(cut, clarity, data=diamonds, geom="blank", group=1) + geom_path()
    qplot(cut, clarity, data=diamonds, geom="blank", group=1) + geom_path(position="jitter")
  }
  
})
