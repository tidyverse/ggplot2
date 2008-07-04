CoordTrans <- proto(CoordCartesian, expr={
  
  muncher <- function(.) TRUE

  munch <- function(., data, npieces=50) {
    data <- add_group(data)
    
    groups <- split(data, data$group)
    munched_groups <- lapply(groups, function(df) .$munch_group(df, npieces))
    do.call("rbind", munched_groups)
  }  
  
  transform <- function(., data) {
    data$x <- .$xtr$transform(data$x)
    data$y <- .$ytr$transform(data$y)
    data
  }
  
  new <- function(., xtrans="identity", ytrans="identity") {
    if (is.character(xtrans)) xtrans <- Trans$find(xtrans)
    if (is.character(ytrans)) ytrans <- Trans$find(ytrans)
  
    .$proto(xtr=xtrans, ytr=ytrans)
  }

  output_set <- function(.) {
    expand <- .$expand()
    list(
      x = expand_range(.$xtr$transform(.$x()$output_set()), expand$x[1], expand$x[2]),
      y = expand_range(.$ytr$transform(.$y()$output_set()), expand$y[1], expand$y[2])
    )
  }

  guide_axes <- function(., theme) {
    range <- .$output_set()
    list(
      x = guide_axis(.$xtr$transform(.$x()$input_breaks()), .$x()$labels(), "bottom", theme),
      y = guide_axis(.$ytr$transform(.$y()$input_breaks()), .$y()$labels(), "left", theme)
    )
  }

  guide_background <- function(., theme) {
    x.major <- unit(.$xtr$transform(.$x()$input_breaks_n()), "native")
    x.minor <- unit(.$xtr$transform(.$x()$output_breaks()), "native")
    y.major <- unit(.$ytr$transform(.$y()$input_breaks_n()), "native")
    y.minor <- unit(.$ytr$transform(.$y()$output_breaks()), "native")
    
    draw_grid(theme, x.minor, x.major, y.minor, y.major)
  }

  # Documentation -----------------------------------------------

  objname <- "trans"
  desc <- "Transformed cartesian coordinate system"
  details <- ""
  icon <- function(.) {
    breaks <- cumsum(1 / 2^(1:5))
    gTree(children=gList(
      segmentsGrob(breaks, 0, breaks, 1),
      segmentsGrob(0, breaks, 1, breaks)
    ))
  }
  
  examples <- function(.) {
    # See ?geom_boxplot for other examples
    
    # Three ways of doing transformating in ggplot:
    #  * by transforming the data
    qplot(log10(carat), log10(price), data=diamonds)
    #  * by transforming the scales
    qplot(carat, price, data=diamonds, log="xy")
    qplot(carat, price, data=diamonds) + scale_x_log10() + scale_y_log10()
    #  * by transforming the coordinate system:
    qplot(carat, price, data=diamonds) + coord_trans(x="log10", y="log10")

    # The difference between transforming the scales and
    # transforming the coordinate system is that scale
    # transformation occurs BEFORE statistics, and coordinate
    # transformation afterwards.  Coordinate transformation also 
    # changes the shape of geoms:
    library(mgcv)
    qplot(carat, price, data=diamonds, log="xy", geom=c("point","smooth"), method="gam", formula=y ~ s(x, bs="cr"))
    qplot(carat, price, data=diamonds, geom=c("point","smooth"), method="gam", formula=y ~ s(x, bs="cr"))  + coord_trans(x="log10", y="log10")
    
    # With a combination of scale and coordinate transformation, it's
    # possible to do back-transformations:
    qplot(carat, price, data=diamonds, log="xy", geom=c("point", "smooth"), method="lm") + coord_trans(x="pow10", y="pow10")
    # cf.
    qplot(carat, price, data=diamonds, geom=c("point", "smooth"), method="lm")
    
  }

  
})


