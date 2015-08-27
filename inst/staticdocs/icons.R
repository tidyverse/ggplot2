# Coords -----------------------------------------------------------------------
coord_cartesian  <- sd_icon({
  gTree(children = gList(
    segmentsGrob(
      c(0, 0.25),
      c(0.25, 0),
      c(1, 0.25),
      c(0.25, 1),
      gp = gpar(col = "grey50", lwd = 0.5)
    ),
    segmentsGrob(
      c(0, 0.75),
      c(0.75, 0),
      c(1, 0.75),
      c(0.75, 1),
      gp = gpar(col = "grey50", lwd = 0.5)
    ),
    segmentsGrob(c(0, 0.5), c(0.5, 0), c(1, 0.5), c(0.5, 1))
  ))
})


coord_fixed  <- sd_icon({
  textGrob("=", gp = gpar(cex = 3))
})


coord_flip  <- sd_icon({
  angles <- seq(0, pi / 2, length.out = 20)[-c(1, 20)]
  gTree(children = gList(
    segmentsGrob(0, 0, 0, 1),
    segmentsGrob(0, 0, 1, 0),
    linesGrob(0.9 * sin(angles), 0.9 * cos(angles),
      arrow = arrow(length = unit(0.05, "npc"))),
    linesGrob(
      0.5 * sin(angles),
      0.5 * cos(angles),
      arrow = arrow(ends = "first", length = unit(0.05, "npc"))
    )
  ))
})

library(maps)
coord_map  <- sd_icon({
  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nz$x <- nz$x - min(nz$x, na.rm = TRUE)
  nz$y <- nz$y - min(nz$y, na.rm = TRUE)
  nz <- nz / max(nz, na.rm = TRUE)
  linesGrob(nz$x, nz$y, default.units = "npc")
})

coord_polar  <- sd_icon({
  circleGrob(r = c(0.1, 0.25, 0.45),  gp = gpar(fill = NA))
})

coord_transform  <- sd_icon({
  breaks <- cumsum(1 / 2 ^ (1:5))
  gTree(children = gList(
    segmentsGrob(breaks, 0, breaks, 1),
    segmentsGrob(0, breaks, 1, breaks)
  ))
})

# Faceting ---------------------------------------------------------------------

facet_grid  <- sd_icon({
  gTree(children = gList(
    rectGrob(
      0,
      1,
      width = 0.95,
      height = 0.05,
      hjust = 0,
      vjust = 1,
      gp = gpar(fill = "grey60", col = NA)
    ),
    rectGrob(
      0.95,
      0.95,
      width = 0.05,
      height = 0.95,
      hjust = 0,
      vjust = 1,
      gp = gpar(fill = "grey60", col = NA)
    ),
    segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
  ))
})

facet_null  <- sd_icon({
  gTree(children = gList(
    rectGrob(
      0,
      1,
      width = 0.95,
      height = 0.05,
      hjust = 0,
      vjust = 1,
      gp = gpar(fill = "grey60", col = NA)
    ),
    rectGrob(
      0.95,
      0.95,
      width = 0.05,
      height = 0.95,
      hjust = 0,
      vjust = 1,
      gp = gpar(fill = "grey60", col = NA)
    ),
    segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
  ))
})

# Geoms ------------------------------------------------------------------------

geom_abline  <- sd_icon(linesGrob(c(0, 1), c(0.2, 0.8)))

geom_bar  <- sd_icon({
  rectGrob(
    c(0.3, 0.7),
    c(0.4, 0.8),
    height = c(0.4, 0.8),
    width = 0.3,
    vjust = 1,
    gp = gpar(fill = "grey20", col = NA)
  )
})

geom_histogram  <- sd_icon({
  y <- c(0.2, 0.3, 0.5, 0.6, 0.2, 0.8, 0.5, 0.3)
  rectGrob(
    seq(0.1, 0.9, by = 0.1),
    y,
    height = y,
    width = 0.1,
    vjust = 1,
    gp = gpar(fill = "grey20", col = NA)
  )
})

geom_boxplot  <- sd_icon({
  gTree(children = gList(
    segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
    rectGrob(
      c(0.3, 0.7),
      c(0.6, 0.8),
      width = 0.3,
      height = c(0.4, 0.4),
      vjust = 1
    ),
    segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
  ))
})

geom_crossbar  <- sd_icon({
  gTree(children = gList(
    rectGrob(
      c(0.3, 0.7),
      c(0.6, 0.8),
      width = 0.3,
      height = c(0.4, 0.4),
      vjust = 1
    ),
    segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
  ))
})

geom_dotplot  <- sd_icon({
  xpos <- c(1, 1, 2, 3, 3, 3, 4, 4, 5, 5, 5, 5, 6, 7, 7, 7, 8, 8, 9) / 10
  ypos <- c(1, 2, 1, 1, 2, 3, 1, 2, 1, 2, 3, 4, 1, 1, 2, 3, 1, 2, 1) / 10
  pointsGrob(
    x = xpos,
    y = ypos,
    pch = 19,
    size = unit(.1, "npc"),
    gp = gpar(col = "black", cex = 0.5),
    default.units = "npc"
  )
})

geom_errorbar  <- sd_icon({
  gTree(children = gList(
    segmentsGrob(c(0.3, 0.7), c(0.3, 0.5), c(0.3, 0.7), c(0.7, 0.9)),
    segmentsGrob(c(0.15, 0.55), c(0.3, 0.5), c(0.45, 0.85), c(0.3, 0.5)),
    segmentsGrob(c(0.15, 0.55), c(0.7, 0.9), c(0.45, 0.85), c(0.7, 0.9))
  ))
})

geom_errorbarh  <- sd_icon({
  gTree(children = gList(
    segmentsGrob(c(0.5, 0.3), c(0.70, 0.30), c(0.9, 0.7), c(0.70, 0.30)),
    segmentsGrob(c(0.5, 0.3), c(0.55, 0.15), c(0.5, 0.3), c(0.85, 0.45)),
    segmentsGrob(c(0.9, 0.7), c(0.55, 0.15), c(0.9, 0.7), c(0.85, 0.45))
  ))
})

geom_freqpoly  <- sd_icon({
  y <- c(0.2, 0.3, 0.5, 0.6, 0.2, 0.8, 0.5, 0.3)
  linesGrob(seq(0.1, 0.9, by = 0.1), y, gp = gpar(col = "grey20"))
})

geom_hline  <- sd_icon({
  linesGrob(c(0, 1), c(0.5, 0.5))
})

geom_linerange  <- sd_icon({
  segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95))
})

geom_path  <- sd_icon({
  linesGrob(c(0.2, 0.4, 0.8, 0.6, 0.5), c(0.2, 0.7, 0.4, 0.1, 0.5))
})

geom_contour  <- sd_icon({
  gTree(children = gList(polygonGrob(
    c(0.45, 0.5, 0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)
  ),
    polygonGrob(
      c(0.25, 0.6, 0.8, 0.5), c(0.5, 0.2, 0.75, 0.9),
      gp = gpar(fill = NA)
    )))
})

geom_density2d  <- sd_icon(inherit = "geom_contour")

geom_line  <- sd_icon({
  pos <- seq(0, 1, length.out = 5)
  linesGrob(pos, c(0.2, 0.7, 0.4, 0.8, 0.3))
})

geom_step  <- sd_icon({
  n <- 15
  xs <- rep(0:n, each = 2)[-2 * (n + 1)] / 15
  ys <- c(0, rep(1:n, each = 2)) / 15
  linesGrob(xs, ys, gp = gpar(col = "grey20"))
})

geom_point  <- sd_icon({
  pos <- seq(0.1, 0.9, length.out = 6)
  pointsGrob(
    x = pos,
    y = pos,
    pch = 19,
    gp = gpar(col = "black", cex = 0.5),
    default.units = "npc"
  )
})

geom_jitter  <- sd_icon({
  pos <- seq(0.1, 0.9, length.out = 6)
  pointsGrob(
    x = pos,
    y = jitter(pos, 3),
    pch = 19,
    gp = gpar(col = "black", cex = 0.5),
    default.units = "npc"
  )
})

geom_pointrange  <- sd_icon({
  gTree(children = gList(
    segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
    pointsGrob(
      c(0.3, 0.7),
      c(0.4, 0.6),
      pch = 19,
      gp = gpar(col = "black", cex = 0.5),
      default.units = "npc"
    )
  ))
})

geom_polygon  <- sd_icon({
  polygonGrob(
    c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3),
    c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3),
    gp = gpar(fill = "grey20", col = NA)
  )
})

geom_quantile  <- sd_icon({
  gTree(children = gList(linesGrob(
    c(0, 0.3, 0.5, 0.8, 1), c(0.8, 0.65, 0.6, 0.6, 0.8)
  ),
    linesGrob(
      c(0, 0.3, 0.5, 0.8, 1), c(0.55, 0.45, 0.5, 0.45, 0.55)
    ),
    linesGrob(
      c(0, 0.3, 0.5, 0.8, 1), c(0.3, 0.25, 0.4, 0.3, 0.2)
    )))
})

geom_raster  <- sd_icon({
  rectGrob(
    c(0.25, 0.25, 0.75, 0.75),
    c(0.25, 0.75, 0.75, 0.25),
    width = 0.5,
    height = c(0.67, 0.5, 0.67, 0.5),
    gp = gpar(col = "grey20", fill = c('#804070', '#668040'))
  )
})

geom_rect  <- sd_icon({
  rectGrob(
    c(0.3, 0.7),
    c(0.4, 0.8),
    height = c(0.4, 0.8),
    width = 0.3,
    vjust = 1,
    gp = gpar(fill = "grey20", col = NA)
  )
})

geom_ribbon  <- sd_icon({
  polygonGrob(
    c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
    c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
    gp = gpar(fill = "grey20", col = NA)
  )
})

geom_area  <- sd_icon({
  polygonGrob(c(0, 0, 0.3, 0.5, 0.8, 1, 1),
    c(0, 1, 0.5, 0.6, 0.3, 0.8, 0),
    gp = gpar(fill = "grey20", col = NA))
})

geom_density  <- sd_icon({
  x <- seq(0, 1, length.out = 80)
  y <- dnorm(x, mean = 0.5, sd = 0.15)
  linesGrob(x, 0.05 + y / max(y) * 0.9, default.units = "npc")
})

geom_segment  <- sd_icon({
  segmentsGrob(c(0.1, 0.3, 0.5, 0.7),
    c(0.3, 0.5, 0.1, 0.9),
    c(0.2, 0.5, 0.7, 0.9),
    c(0.8, 0.7, 0.4, 0.3))
})

geom_smooth  <- sd_icon({
  gTree(children = gList(polygonGrob(
    c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
    c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
    gp = gpar(fill = "grey60", col = NA)
  ),
    linesGrob(
      c(0, 0.3, 0.5, 0.8, 1), c(0.6, 0.4, 0.5, 0.4, 0.6)
    )))
})

geom_text  <- sd_icon({
  textGrob("text", rot = 45, gp = gpar(cex = 1.2))
})

geom_tile  <- sd_icon({
  rectGrob(
    c(0.25, 0.25, 0.75, 0.75),
    c(0.25, 0.75, 0.75, 0.25),
    width = 0.5,
    height = c(0.67, 0.5, 0.67, 0.5),
    gp = gpar(col = "grey20", fill = c('#804070', '#668040'))
  )
})

geom_violin  <- sd_icon({
  y <- seq(-.3, .3, length.out = 40)
  x1 <- dnorm(y, mean = -.15, sd = 0.05) +
    1.5 * dnorm(y, mean = 0.1, sd = 0.1)
  x2 <-
    dnorm(y, mean = -.1, sd = 0.1) + dnorm(y, mean = 0.1, sd = 0.1)

  y <- c(y, rev(y))
  x1 <- c(x1,-rev(x1)) / max(8 * x1)
  x2 <- c(x2,-rev(x2)) / max(8 * x2)
  gp <- gpar(fill = "black")
  gTree(children = gList(
    polygonGrob(x1 + .30, y + .35, default.units = "npc", gp = gp),
    polygonGrob(x2 + .70, y + .55, default.units = "npc", gp = gp)
  ))
})

geom_vline  <- sd_icon({
  linesGrob(c(0.5, 0.5), c(0, 1))
})

# Position adjustments --------------------------------------------------------

position_dodge  <- sd_icon({
  y <- c(0.5, 0.3)
  rectGrob(
    c(0.25, 0.75),
    y,
    width = 0.4,
    height = y,
    gp = gpar(col = "grey60", fill = c('#804070', '#668040')),
    vjust = 1
  )
})

position_fill  <- sd_icon({
  y <- c(0.5, 0.8)
  rectGrob(
    0.5,
    c(0.625, 1),
    width = 0.4,
    height = c(0.625, 0.375),
    gp = gpar(col = "grey60", fill = c('#804070', '#668040')),
    vjust = 1
  )
})

position_identity  <- sd_icon({
  rectGrob(
    0.5,
    c(0.5, 0.3),
    width = 0.4,
    height = c(0.5, 0.3),
    gp = gpar(col = "grey60", fill = c('#804070', '#668040')),
    vjust = 1
  )
})

position_jitter  <- sd_icon(inherit = "geom_jitter")

position_stack  <- sd_icon({
  y <- c(0.5, 0.8)
  rectGrob(
    0.5,
    c(0.5, 0.8),
    width = 0.4,
    height = c(0.5, 0.3),
    gp = gpar(col = "grey60", fill = c('#804070', '#668040')),
    vjust = 1
  )
})

# Scales -----------------------------------------------------------------------

scale_alpha  <- sd_icon({
  x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  rectGrob(x,
    width = 0.25,
    gp = gpar(fill = scales::alpha("black", x), col = NA))
})

scale_colour_brewer  <- sd_icon({
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = RColorBrewer::brewer.pal(5, "PuOr"), col = NA)
  )
})

scale_colour_gradient  <- sd_icon({
  g <- scale_fill_gradient()
  g$train(1:5)
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = g$map(1:5), col = NA)
  )
})

scale_colour_gradient2  <- sd_icon({
  g <- scale_fill_gradient2()
  g$train(1:5 - 3)
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = g$map(1:5 - 3), col = NA)
  )
})

scale_colour_gradientn  <- sd_icon({
  g <- scale_fill_gradientn(colours = rainbow(7))
  g$train(1:5)
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = g$map(1:5), col = NA)
  )
})

scale_colour_grey  <- sd_icon({
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = gray(seq(0, 1, length.out = 5)), col = NA))
})

scale_colour_hue  <- sd_icon({
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = hcl(
      seq(0, 360, length.out = 6)[-6], c = 100, l = 65
    ), col = NA))
})

scale_identity  <- sd_icon({
  textGrob("f(x) = x", gp = gpar(cex = 1.2))
})

scale_linetype  <- sd_icon({
  gTree(children = gList(
    segmentsGrob(0, 0.25, 1, 0.25, gp = gpar(lty = 1)),
    segmentsGrob(0, 0.50, 1, 0.50, gp = gpar(lty = 2)),
    segmentsGrob(0, 0.75, 1, 0.75, gp = gpar(lty = 3))
  ))
})

scale_manual  <- sd_icon({
  textGrob("DIY", gp = gpar(cex = 1.2))
})

scale_shape  <- sd_icon({
  gTree(children = gList(
    circleGrob(0.7, 0.7, r = 0.1),
    segmentsGrob(0.2, 0.3, 0.4, 0.3),
    segmentsGrob(0.3, 0.2, 0.3, 0.4),
    polygonGrob(c(0.2, 0.2, 0.4, 0.4), c(0.8, 0.6, 0.6, 0.8)),
    polygonGrob(c(0.6, 0.7, 0.8), c(0.2, 0.4, 0.2))
  ))
})

scale_size  <- sd_icon({
  pos <- c(0.15, 0.3, 0.5, 0.75)
  circleGrob(pos,
    pos,
    r = (c(0.1, 0.2, 0.3, 0.4) / 2.5),
    gp = gpar(fill = "grey50", col = NA))
})

scale_x_date  <- sd_icon({
  textGrob("14/10/1979", gp = gpar(cex = 1))
})

scale_x_datetime  <- sd_icon({
  textGrob("14/10/1979\n10:14am", gp = gpar(cex = 0.9))
})

# Statistics -------------------------------------------------------------------

stat_bin  <- sd_icon(inherit = "geom_histogram")

stat_bindot  <- sd_icon(inherit = "geom_dotplot")

stat_boxplot  <- sd_icon(inherit = "geom_boxplot")

stat_contour  <- sd_icon(inherit = "geom_contour")

stat_density2d  <- sd_icon(inherit = "geom_density2d")

stat_ecdf  <- sd_icon(inherit = "geom_step")

stat_density  <- sd_icon(inherit = "geom_density")

stat_identity  <- sd_icon({
  textGrob('f(x) = x', gp = gpar(cex = 1.2))
})

stat_quantile  <- sd_icon(inherit = "geom_quantile")

stat_smooth  <- sd_icon(inherit = "geom_smooth")

stat_sum  <- sd_icon({
  textGrob(expression(Sigma), gp = gpar(cex = 4))
})

# The line stats will be removed in the future
stat_abline  <- sd_icon(inherit = "geom_abline")

stat_vline  <- sd_icon(inherit = "geom_vline")

stat_hline  <- sd_icon(inherit = "geom_hline")

stat_ydensity  <- sd_icon(inherit = "geom_violin")
