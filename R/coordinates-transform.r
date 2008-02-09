CoordTransform <- proto(CoordCartesian, expr={
  
  muncher <- function(.) TRUE
  munch_group <- function(., data, npieces=50) {
    n <- nrow(data)

    x <- approx(data$x, n = npieces * (n - 1) + 1)$y
    y <- approx(data$y, n = npieces * (n - 1) + 1)$y
    
    cbind(
      .$transform(data.frame(x=x, y=y)),
      data[c(rep(1:(n-1), each=npieces), n), setdiff(names(data), c("x", "y"))]
    )
  }
  
  munch <- function(., data, npieces=50) {
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

  frange <- function(.) {
    expand <- .$expand()
    list(
      x = expand_range(.$xtr$transform(.$x()$frange()), expand$x[1], expand$x[2]),
      y = expand_range(.$ytr$transform(.$y()$frange()), expand$y[1], expand$y[2])
    )
  }

  guide_axes <- function(.) {
    range <- .$frange()
    list(
      x = ggaxis(.$xtr$transform(.$x()$breaks()), .$x()$labels(), "bottom", range$x),
      y = ggaxis(.$ytr$transform(.$y()$breaks()), .$y()$labels(), "left", range$y)
    )
  }

  guide_inside <- function(., plot) {
    gp <- gpar(fill=plot$grid.fill, col=plot$grid.colour)
    ggname("grill", gTree(children = gList(
      ggname("background", rectGrob(gp=gpar(fill=plot$grid.fill, col=NA))),

      ggname("minor-vertical", segmentsGrob(.$xtr$transform(.$x()$minor_breaks()), unit(0, "npc"), .$xtr$transform(.$x()$minor_breaks()), unit(1, "npc"), gp = gpar(col=plot$grid.minor.colour, lwd=0.5), default.units="native")),
      ggname("major-vertical", segmentsGrob(.$xtr$transform(.$x()$breaks()), unit(0, "npc"), .$xtr$transform(.$x()$breaks()), unit(1, "npc"), gp = gp, default.units="native")),

      ggname("minor-horizontal", segmentsGrob(unit(0, "npc"), .$ytr$transform(.$y()$minor_breaks()), unit(1, "npc"), .$ytr$transform(.$y()$minor_breaks()), gp = gpar(col=plot$grid.minor.colour, lwd=0.5), default.units="native")),
      ggname("major-horizontal",segmentsGrob(unit(0, "npc"), .$ytr$transform(.$y()$breaks()), unit(1, "npc"), .$ytr$transform(.$y()$breaks()), gp = gp, default.units="native")),

      ggname("border", rectGrob(gp=gpar(col=plot$grid.colour, lwd=3, fill=NA)))
    )))
  }

  # Documetation -----------------------------------------------

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


