ScaleIdentity <- proto(ScaleDiscrete, {  
  doc <- TRUE
  common <- c("alpha", "colour","fill","size","shape","linetype")
  aliases <- "scale_color_identity"
  new <- function(., name=NULL, breaks=NULL, labels=NULL, formatter = NULL, legend = TRUE, variable="x") {
    
    b_and_l <- check_breaks_and_labels(breaks, labels)
#    legend <- legend && !is.null(b_and_l$labels)
    
    .$proto(name=name, breaks=b_and_l$breaks, .labels=b_and_l$labels, .input=variable, .output=variable, formatter = formatter, legend = legend)
  }

  train <- function(., data, drop = FALSE) {
    .$breaks <- union(.$breaks, unique(data))
    if (is.numeric(data)) {
      if (all(is.na(data)) || all(!is.finite(data))) return()
      .$.domain <- range(data, .$.domain, na.rm=TRUE, finite=TRUE)
    } else {
      .$.domain <- discrete_range(.$.domain, data, drop = drop)
    }
  }

  map_df <- function(., data) {
    if (!all(.$input() %in% names(data))) return(data.frame())
    data[, .$input(), drop=FALSE]
  }
  output_breaks <- function(.) .$breaks
  labels <- function(.) {
    if (!is.null(.$.labels)) return(as.list(.$.labels))

    if (is.null(get("formatter", .))) {
      f <- match.fun(identity)
    } else {
      f <- match.fun(get("formatter", .))
    }
    as.list(f(.$input_breaks()))
  }

  # Documentation -----------------------------------------------

  objname <- "identity"
  desc <- "Use values without scaling"
  icon <- function(.) textGrob("f(x) = x", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    colour <- c("red", "green", "blue", "yellow")
    qplot(1:4, 1:4, fill = colour, geom = "tile")
    qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()
    
    # To get a legend, you also need to supply the labels to
    # be used on the legend
    qplot(1:4, 1:4, fill = colour, geom = "tile") +
      scale_fill_identity("trt", labels = letters[1:4], breaks = colour)
    
    # cyl scaled to appropriate size
    qplot(mpg, wt, data = mtcars, size = cyl)

    # cyl used as point size
    qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
  }
  
})
