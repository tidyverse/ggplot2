
ScaleIdentity <- proto(ScaleDiscrete, {  
  common <- c("colour","fill","size","shape","linetype")
  new <- function(., name=NULL, breaks=NULL, labels=NULL, grob="point", variable="x") {
    .$proto(name=name, .breaks=breaks, .labels=labels, .grob=grob, .input=variable, .output=variable)
  }

  guides.manual <- function(scale, ...) {
    if(is.null(scale$labels)) return()
    guides.default(scale, ...)
  }
  
  train <- function(., data) {
    .$.breaks <- union(.$.breaks, unique(data))
  }
  map_df <- function(., data) {
    if (!all(.$input() %in% names(data))) return(data.frame())
    data[, .$input(), drop=FALSE]
  }
  breaks <- function(.) .$.breaks
  labels <- function(.) .$.labels

  guide_legend_geom <- function(.) Geom$find(.$.grob)
  
  guide_legend <- function(.) {
    if (is.null(.$.labels)) return()
    .super$guide_legend(.)
  }
  
  # Documetation -----------------------------------------------

  objname <- "identity"
  desc <- "Don't remap values, use directly"
  icon <- function(.) textGrob("f(x) = x", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    colour <- c("red","green","blue","yellow")
    qplot(1:4, 1:4, fill=colour, geom="tile")
    qplot(1:4, 1:4, fill=colour, geom="tile") + scale_fill_identity()
    
    # To get a legend, you also need to supply the labels to
    # be used on the legend, and the grob to draw them:
    # grob_tile, grob_line, or grob_point
    qplot(1:4, 1:4, fill=colour, geom="tile") + scale_fill_identity(labels=letters[1:4], grob="tile", name="trt")
    
    # cyl scaled to appropriate size
    qplot(mpg, wt, data=mtcars, size = cyl)

    # cyl used as point size
    qplot(mpg, wt, data=mtcars, size = cyl) + scale_size_identity()
  
  }
  
})
