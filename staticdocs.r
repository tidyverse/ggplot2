library(staticdocs)
list(
  index = list(
    name        = "Geoms",
    description = "Geoms, short for geometric objects, describe the type of plot you will produce.",  

    items = list(
      list(
        name        = "geom_abline",
        description = "Line, specified by slope and intercept" ),
      list(
        name        = "geom_area",
        description = "Area plots" ),
      list(
        name        = "geom_bar",
        description = "Bars, rectangles with bases on y-axis" ),
      list(
        name        = "geom_bin2d",
        description = "Add heatmap of 2d bin counts" ),
      list(
        name        = "geom_blank",
        description = "Blank, draws nothing" ),
      list(
        name        = "geom_boxplot",
        description = "Box and whiskers plot" ),
      list(
        name        = "geom_contour",
        description = "Display contours of a 3d surface in 2d" ),
      list(
        name        = "geom_crossbar",
        description = "Hollow bar with middle indicated by horizontal line" ),
      list(
        name        = "geom_density",
        description = "Display a smooth density estimate" ),
      list(
        name        = "geom_density2d",
        description = "Contours from a 2d density estimate" ),
      list(
        name        = "geom_errorbar",
        description = "Error bars" ),
      list(
        name        = "geom_errorbarh",
        description = "Horizontal error bars" ),
      list(
        name        = "geom_freqpoly",
        description = "Frequency polygon" ),
      list(
        name        = "geom_hex",
        description = "Tile the plane with hexagons" ),
      list(
        name        = "geom_histogram",
        description = "Histogram" ),
      list(
        name        = "geom_hline",
        description = "Line, horizontal" ),
      list(
        name        = "geom_jitter",
        description = "Points, jittered to reduce overplotting" ),
      list(
        name        = "geom_line",
        description = "Connect observations, in ordered by x value" ),
      list(
        name        = "geom_linerange",
        description = "An interval represented by a vertical line" ),
      list(
        name        = "geom_path",
        description = "Connect observations, in original order" ),
      list(
        name        = "geom_point",
        description = "Points, as for a scatterplot" ),
      list(
        name        = "geom_pointrange",
        description = "An interval represented by a vertical line, with a point in the middle" ),
      list(
        name        = "geom_polygon",
        description = "Polygon, a filled path" ),
      list(
        name        = "geom_quantile",
        description = "Add quantile lines from a quantile regression" ),
      list(
        name        = "geom_rect",
        description = "2d rectangles" ),
      list(
        name        = "geom_ribbon",
        description = "Ribbons, y range with continuous x values" ),
      list(
        name        = "geom_rug",
        description = "Marginal rug plots" ),
      list(
        name        = "geom_segment",
        description = "Single line segments" ),
      list(
        name        = "geom_smooth",
        description = "Add a smoothed condition mean" ),
      list(
        name        = "geom_step",
        description = "Connect observations by stairs" ),
      list(
        name        = "geom_text",
        description = "Textual annotations" ),
      list(
        name        = "geom_tile",
        description = "Tile plot as densely as possible, assuming that every tile is the same size" ),
      list(
        name        = "geom_vline",
        description = "Line, vertical" )
    )
  ), 

  list(
    name        = "Statistics",
    description = "It's often useful to transform your data before plotting, and that's what statistical transformations do.",

    items = list (
      list(
        name        = "stat_abline",
        description = "Add a line with slope and intercept" ),
      list(
        name        = "stat_bin",
        description = "Bin data" ),
      list(
        name        = "stat_bin2d",
        description = "Bin 2d plane into rectangles" ),
      list(
        name        = "stat_binhex",
        description = "Bin 2d plane into hexagons" ),
      list(
        name        = "stat_boxplot",
        description = "Calculate components of box and whisker plot" ),
      list(
        name        = "stat_contour",
        description = "Contours of 3d data" ),
      list(
        name        = "stat_density",
        description = "Density estimation, 1D" ),
      list(
        name        = "stat_density2d",
        description = "Density estimation, 2D" ),
      list(
        name        = "stat_function",
        description = "Superimpose a function" ),
      list(
        name        = "stat_hline",
        description = "Add a horizontal line" ),
      list(
        name        = "stat_identity",
        description = "Don't transform data" ),
      list(
        name        = "stat_qq",
        description = "Calculation for quantile-quantile plot" ),
      list(
        name        = "stat_quantile",
        description = "Continuous quantiles" ),
      list(
        name        = "stat_smooth",
        description = "Add a smoother" ),
      list(
        name        = "stat_spoke",
        description = "Convert angle and radius to xend and yend" ),
      list(
        name        = "stat_sum",
        description = "Sum unique values. Useful for overplotting on scatterplots" ),
      list(
        name        = "stat_summary",
        description = "Summarise y values at every unique x" ),
      list(
        name        = "stat_unique",
        description = "Remove duplicates" ),
      list(
        name        = "stat_vline",
        description = "Add a vertical line" )
    )
  ),

  list(
    name        = "Scales",
    description = "Scales control the mapping between data and aesthetics.",

    items = list(
      list(
        name        = "scale_alpha",
        description = "Alpha scale for continuous variable" ),
      list(
        name        = "scale_continuous",
        description = "Continuous position scale" ),
      list(
        name        = "scale_datetime",
        description = "Position scale, date time" ),
      list(
        name        = "scale_gradient",
        description = "Smooth gradient between two colours" ),
      list(
        name        = "scale_gradientn",
        description = "Smooth gradient between n colours" ),
      list(
        name        = "scale_hue",
        description = "Qualitative colour scale with evenly spaced hues" ),
      list(
        name        = "scale_linetype",
        description = "Scale for line patterns" ),
      list(
        name        = "scale_shape",
        description = "Scale for shapes, aka glyphs" ),
      list(
        name        = "scale_brewer",
        description = "Sequential, diverging and qualitative colour scales from colorbrewer.org" ),
      list(
        name        = "scale_date",
        description = "Position scale, date" ),
      list(
        name        = "scale_discrete",
        description = "Discrete position scale" ),
      list(
        name        = "scale_gradient2",
        description = "Smooth gradient between three colours (high, low and midpoints)" ),
      list(
        name        = "scale_grey",
        description = "Sequential grey colour scale" ),
      list(
        name        = "scale_identity",
        description = "Use values without scaling" ),
      list(
        name        = "scale_manual",
        description = "Create your own discrete scale" ),
      list(
        name        = "scale_size",
        description = "Size scale for continuous variable" )
    )
  ),

  list(
    name        = "Coordinate systems",
    description = "Coordinate systems adjust the mapping from coordinates to the 2d plane of the computer screen.",

    items = list(
      list(
        name        = "coord_cartesian",
        description = "Cartesian coordinates" ),
      list(
        name        = "coord_equal",
        description = "Equal scale cartesian coordinates" ),
      list(
        name        = "coord_flip",
        description = "Flipped cartesian coordinates" ),
      list(
        name        = "coord_map",
        description = "Map projections" ),
      list(
        name        = "coord_polar",
        description = "Polar coordinates" ),
      list(
        name        = "coord_trans",
        description = "Transformed cartesian coordinate system" )
    )
  ),

  list(
    name        = "Faceting",
    description = "Facets display subsets of the dataset in different panels.",

    items = list(
      list(
        name        = "facet_grid",
        description = "Lay out panels in a rectangular/tabular manner." ),
      list(
        name        = "facet_wrap",
        description = "Wrap a 1d ribbon of panels into 2d." )
    )
  ),

  list(
    name        = "Position adjustments",
    description = "Position adjustments can be used to fine tune positioning of objects to achieve effects like dodging, jittering and stacking.",

    items = list(
      list(
        name        = "position_dodge",
        description = "Adjust position by dodging overlaps to the side" ),
      list(
        name        = "position_fill",
        description = "Stack overlapping objects on top of one another, and standardise have equal height" ),
      list(
        name        = "position_identity",
        description = "Don't adjust position" ),
      list(
        name        = "position_stack",
        description = "Stack overlapping objects on top of one another" ),
      list(
        name        = "position_jitter",
        description = "Jitter points to avoid overplotting" )
    )
  ), 
  icons = list(  
    coord_polar = sd_icon({
      circleGrob(r = c(0.1, 0.25, 0.45),  gp = gpar(fill = NA))
    }),
    coord_transform = sd_icon({
      breaks <- cumsum(1 / 2^(1:5))
      gTree(children = gList(
        segmentsGrob(breaks, 0, breaks, 1),
        segmentsGrob(0, breaks, 1, breaks)
      ))
    }),
    geom_abline = sd_icon(linesGrob(c(0, 1), c(0.2, 0.8))),
    geom_bar = sd_icon({
      rectGrob(c(0.3, 0.7), c(0.4, 0.8), height = c(0.4, 0.8), width = 0.3, 
        vjust = 1, gp = gpar(fill = "grey20", col = NA))
      }),
    geom_histogram = sd_icon({
      y <- c(0.2, 0.3, 0.5, 0.6,0.2, 0.8, 0.5, 0.3)
      rectGrob(seq(0.1, 0.9, by = 0.1), y, height = y, width = 0.1, vjust = 1,
        gp = gpar(fill = "grey20", col = NA))
    }),
    geom_boxplot = sd_icon({
      gTree(children = gList(
        segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
        rectGrob(c(0.3, 0.7), c(0.6, 0.8), width = 0.3, height = c(0.4, 0.4),
          vjust = 1),
        segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
      ))
    }),
    geom_crossbar = sd_icon({
      gTree(children = gList(
        rectGrob(c(0.3, 0.7), c(0.6, 0.8), width = 0.3, height = c(0.4, 0.4), vjust = 1),
        segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
      ))
    }),
    geom_dotplot = sd_icon({
      xpos <- c(1,1,2,3,3,3,4,4,5,5,5,5,6,7,7,7,8,8,9)/10
      ypos <- c(1,2,1,1,2,3,1,2,1,2,3,4,1,1,2,3,1,2,1)/10
      pointsGrob(x = xpos, y = ypos, pch = 19, size = unit(.1, "npc"),
                 gp = gpar(col = "black", cex = 0.5), default.units = "npc")
    }),
    geom_errorbar = sd_icon({
      gTree(children = gList(
        segmentsGrob(c(0.3, 0.7), c(0.3, 0.5), c(0.3, 0.7), c(0.7, 0.9)),
        segmentsGrob(c(0.15, 0.55), c(0.3, 0.5), c(0.45, 0.85), c(0.3, 0.5)),
        segmentsGrob(c(0.15, 0.55), c(0.7, 0.9), c(0.45, 0.85), c(0.7, 0.9))
      ))
    }),
    geom_errorbarh = sd_icon({
      gTree(children = gList(
        segmentsGrob(c(0.5, 0.3), c(0.70, 0.30), c(0.9, 0.7), c(0.70, 0.30)),
        segmentsGrob(c(0.5, 0.3), c(0.55, 0.15), c(0.5, 0.3), c(0.85, 0.45)),
        segmentsGrob(c(0.9, 0.7), c(0.55, 0.15), c(0.9, 0.7), c(0.85, 0.45))
      ))
    }),
    geom_freqpoly = sd_icon({
      y <- c(0.2, 0.3, 0.5, 0.6,0.2, 0.8, 0.5, 0.3)
      linesGrob(seq(0.1, 0.9, by = 0.1), y, gp = gpar(col = "grey20"))
    }),
    geom_hline = sd_icon({
      linesGrob(c(0, 1), c(0.5, 0.5))
    }),
    geom_linerange = sd_icon({
      segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95))
    }),
    geom_path = sd_icon({
      linesGrob(c(0.2, 0.4, 0.8, 0.6, 0.5), c(0.2, 0.7, 0.4, 0.1, 0.5))
    }),
    geom_contour = sd_icon({
      gTree(children = gList(
        polygonGrob(c(0.45,0.5,0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)),
        polygonGrob(c(0.25,0.6,0.8, 0.5), c(0.5, 0.2, 0.75, 0.9), 
          gp = gpar(fill = NA))
      ))
    }),
    geom_density2d = sd_icon(inherit = "geom_contour"),
    geom_line = sd_icon({
      pos <- seq(0, 1, length = 5)
      linesGrob(pos, c(0.2, 0.7, 0.4, 0.8, 0.3))
    }),
    geom_step = sd_icon({
      n <- 15
      xs <- rep(0:n, each = 2)[-2*(n + 1)] / 15
      ys <- c(0, rep(1:n, each = 2)) / 15
      linesGrob(xs, ys, gp = gpar(col = "grey20"))
    }),
    geom_point = sd_icon({
      pos <- seq(0.1, 0.9, length = 6)
      pointsGrob(x = pos, y = pos, pch = 19,
        gp = gpar(col = "black", cex = 0.5), default.units = "npc")
    }),
    geom_jitter = sd_icon({
      pos <- seq(0.1, 0.9, length = 6)
      pointsGrob(x = pos, y = jitter(pos, 3), pch = 19,
        gp = gpar(col = "black", cex = 0.5), default.units = "npc")
    }),
    geom_pointrange = sd_icon({
      gTree(children = gList(
        segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
        pointsGrob(c(0.3, 0.7), c(0.4, 0.6), pch = 19,
          gp = gpar(col = "black", cex = 0.5), default.units = "npc")
      ))
    }),
    geom_polygon = sd_icon({
      polygonGrob(c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3),
      c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3), gp = gpar(fill = "grey20", col = NA))
    }),
    geom_quantile = sd_icon({
      gTree(children = gList(
        linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.8, 0.65, 0.6, 0.6, 0.8)),
        linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.55, 0.45, 0.5, 0.45, 0.55)),
        linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.3, 0.25, 0.4, 0.3, 0.2))
      ))
    }),
    geom_raster = sd_icon({
      rectGrob(c(0.25, 0.25, 0.75, 0.75), c(0.25, 0.75, 0.75, 0.25),
        width = 0.5, height = c(0.67, 0.5, 0.67, 0.5), 
        gp = gpar(col = "grey20", fill = c('#804070', '#668040')))
    }),
    geom_rect = sd_icon({
      rectGrob(c(0.3, 0.7), c(0.4, 0.8), height = c(0.4, 0.8), width = 0.3,
        vjust = 1, gp = gpar(fill = "grey20", col = NA))
    }),
    geom_ribbon = sd_icon({
      polygonGrob(c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
        c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
        gp = gpar(fill = "grey20", col = NA))
    }),
    geom_area = sd_icon({
      polygonGrob(c(0, 0,0.3, 0.5, 0.8, 1, 1),
        c(0, 1,0.5, 0.6, 0.3, 0.8, 0),
        gp = gpar(fill = "grey20", col = NA))
    }),
    geom_density = sd_icon({
      x <- seq(0, 1, length = 80)
      y <- dnorm(x, mean = 0.5, sd = 0.15)
      linesGrob(x, 0.05 + y / max(y) * 0.9, default = "npc")
    }),
    geom_segment = sd_icon({
      segmentsGrob(c(0.1, 0.3, 0.5, 0.7), c(0.3, 0.5, 0.1, 0.9),
        c(0.2, 0.5, 0.7, 0.9), c(0.8, 0.7, 0.4, 0.3))
    }),
    geom_smooth = sd_icon({
      gTree(children = gList(
        polygonGrob(c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
          c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
          gp = gpar(fill = "grey60", col = NA)),
        linesGrob(c(0, 0.3, 0.5, 0.8, 1), c(0.6, 0.4, 0.5, 0.4, 0.6))
      ))
    }),
    geom_text = sd_icon({
      textGrob("text", rot = 45, gp = gpar(cex = 1.2))
    }),
    geom_tile = sd_icon({
      rectGrob(c(0.25, 0.25, 0.75, 0.75), c(0.25, 0.75, 0.75, 0.25),
        width = 0.5, height = c(0.67, 0.5, 0.67, 0.5),
        gp = gpar(col = "grey20", fill = c('#804070', '#668040')))
    }),
    geom_violin = sd_icon({
      y <- seq(-.3, .3, length = 40)
      x1 <- dnorm(y, mean = -.15, sd = 0.05) +
       1.5 * dnorm(y, mean = 0.1, sd = 0.1)
      x2 <- dnorm(y, mean = -.1, sd = 0.1) + dnorm(y, mean = 0.1, sd = 0.1)

      y <- c(y, rev(y))
      x1 <- c(x1, -rev(x1)) / max(8 * x1)
      x2 <- c(x2, -rev(x2)) / max(8 * x2)
      gp <- gpar(fill = "black")
      gTree(children = gList(
        polygonGrob(x1 + .30, y + .35, default = "npc", gp = gp),
        polygonGrob(x2 + .70, y + .55, default = "npc", gp = gp))
      )
    }),
    geom_vline = sd_icon({
      linesGrob(c(0.5, 0.5), c(0, 1))
    }),
    position_dodge = sd_icon({
      y <- c(0.5, 0.3)
      rectGrob(c(0.25, 0.75), y, width = 0.4, height = y,
        gp = gpar(col = "grey60", fill = c('#804070', '#668040')), vjust = 1)
    }),
    position_fill = sd_icon({
      y <- c(0.5, 0.8)
      rectGrob(0.5, c(0.625, 1), width = 0.4, height = c(0.625, 0.375),
        gp = gpar(col = "grey60", fill = c('#804070', '#668040')), vjust = 1)
    }),
    position_identity = sd_icon({
      rectGrob(0.5, c(0.5, 0.3), width = 0.4, height = c(0.5, 0.3),
        gp = gpar(col = "grey60", fill = c('#804070', '#668040')), vjust = 1) 
    }),
    position_jitter = sd_icon(inherit = "geom_jitter" ),
    position_stack = sd_icon({
      y <- c(0.5, 0.8)
      rectGrob(0.5, c(0.5, 0.8), width = 0.4, height = c(0.5, 0.3),
        gp = gpar(col = "grey60", fill = c('#804070', '#668040')), vjust = 1)
    }),
    stat_bin = sd_icon(inherit = "geom_histogram" ),
    stat_bindot = sd_icon(inherit = "geom_dotplot" ),
    stat_boxplot = sd_icon(inherit = "geom_boxplot" ),
    stat_contour = sd_icon(inherit = "geom_contour" ),
    stat_density2d = sd_icon(inherit = "geom_density2d" ),
    stat_density = sd_icon(inherit = "geom_density" ),
    stat_identity = sd_icon({
      textGrob('f(x) = x', gp = gpar(cex = 1.2))
    }),
    stat_quantile = sd_icon(inherit = "geom_quantile" ),
    stat_smooth = sd_icon(inherit = "geom_smooth" ),
    stat_sum = sd_icon({
      textGrob(expression(Sigma), gp = gpar(cex = 4))
    }),
    # The line stats will be removed in the future
    stat_abline = sd_icon(inherit = "geom_abline" ),
    stat_vline = sd_icon(inherit = "geom_vline" ),
    stat_hline = sd_icon(inherit = "geom_hline" ),
    stat_ydensity = sd_icon(inherit = "geom_violin" )
))