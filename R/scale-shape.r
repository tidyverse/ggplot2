ScaleShape <- proto(ScaleDiscrete, expr={
  doc <- TRUE
  common <- NULL
  .input <- .output <- "shape"
  desc <- "Point glyph shapes"
  solid <- TRUE

  new <- function(., name=NULL, solid=TRUE, limits = NULL, breaks = NULL, labels = NULL, formatter = identity, legend = TRUE) {
    
    b_and_l <- check_breaks_and_labels(breaks, labels)
    .$proto(name=name, solid=solid, limits = b_and_l$limits, breaks = b_and_l$breaks, .labels = labels, formatter = formatter, legend = legend)
  }
  
  output_set <- function(.) {
    (if (.$solid) {
      c(16, 17, 15, 3, 7, 8)
    } else {
      c(1, 2, 0, 3, 7, 8)
    })[1:length(.$input_set())]
  }

  max_levels <- function(.) 6
  
  # Documentation -----------------------------------------------
  objname <- "shape"
  desc <- "Scale for shapes, aka glyphs"
  icon <- function(.) {
    gTree(children=gList(
      circleGrob(0.7, 0.7, r=0.1),
      segmentsGrob(0.2, 0.3, 0.4, 0.3),
      segmentsGrob(0.3, 0.2, 0.3, 0.4),
      polygonGrob(c(0.2, 0.2, 0.4, 0.4), c(0.8, 0.6, 0.6, 0.8)),
      polygonGrob(c(0.6, 0.7, 0.8), c(0.2, 0.4, 0.2))
    ))
  }
  
  examples <- function(.) {
    dsmall <- diamonds[sample(nrow(diamonds), 100), ]
    
    (d <- qplot(carat, price, data=dsmall, shape=cut))
    d + scale_shape(solid = TRUE) # the default
    d + scale_shape(solid = FALSE)
    d + scale_shape(name="Cut of diamond")
    d + scale_shape(name="Cut of\ndiamond")
    
    # To change order of levels, change order of 
    # underlying factor
    levels(dsmall$cut) <- c("Fair", "Good", "Very Good", "Premium", "Ideal")

    # Need to recreate plot to pick up new data
    qplot(price, carat, data=dsmall, shape=cut)

    # Or for short:
    d %+% dsmall
    
  }
}) 
