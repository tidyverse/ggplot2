library(grid)
library(maps)
library(ggplot2)

set.seed(1014)

# Coords -----------------------------------------------------------------------

write_icon <- function(name, code) {
  path <- paste0("icons/", name, ".png")
  png(path, width = 60, height = 60, pointsize = 18)
  on.exit(dev.off())

  grid.draw(code)
  invisible()
}

write_icon("coord_cartesian", {
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

write_icon("coord_fixed", {
  textGrob("=", gp = gpar(cex = 3))
})

write_icon("coord_flip", {
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

write_icon("coord_map", {
  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nz$x <- nz$x - min(nz$x, na.rm = TRUE)
  nz$y <- nz$y - min(nz$y, na.rm = TRUE)
  nz <- nz / max(nz, na.rm = TRUE)
  linesGrob(nz$x, nz$y, default.units = "npc")
})

file.copy("icons/coord_map.png", "icons/geom_map.png")
file.copy("icons/coord_map.png", "icons/geom_sf.png")

write_icon("coord_polar", {
  circleGrob(r = c(0.1, 0.25, 0.45),  gp = gpar(fill = NA))
})

write_icon("coord_transform", {
  breaks <- cumsum(1 / 2 ^ (1:5))
  gTree(children = gList(
    segmentsGrob(breaks, 0, breaks, 1),
    segmentsGrob(0, breaks, 1, breaks)
  ))
})

# Faceting ---------------------------------------------------------------------

write_icon("facet_grid", {
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

write_icon("facet_wrap", {
  gTree(children = gList(
    rectGrob(
      0,
      c(0.49, 1),
      width = 1,
      height = 0.05,
      hjust = 0,
      vjust = 1,
      gp = gpar(fill = "grey60", col = NA)
    ),
    segmentsGrob(c(0, 0.5), c(0.5, 0), c(1, 0.5), c(0.5, 1))
  ))
})

# Geoms ------------------------------------------------------------------------

write_icon("geom_abline", linesGrob(c(0, 1), c(0.2, 0.8)))

write_icon("geom_bar", {
  rectGrob(
    c(0.3, 0.7),
    c(0.4, 0.8),
    height = c(0.4, 0.8),
    width = 0.3,
    vjust = 1,
    gp = gpar(fill = "grey20", col = NA)
  )
})

write_icon("geom_bin2d", {
  n <- 5
  x <- seq(0, 1, length.out = n + 1)[-(n + 1)]
  out <- expand.grid(x = x, y = x)
  fill <- sqrt((out$x - 0.5) ^ 2 + (out$y - 0.5) ^ 2)

  pal <- scales::pal_seq_gradient("#56B1F7", "#132B43")
  rectGrob(
    out$x + 1/n/2,
    out$y + 1/n/2,
    width = 1/n,
    height = 1/n,
    gp = gpar(col = "grey20", fill = pal(scales::rescale(fill)))
  )
})

write_icon("geom_blank", {
  rectGrob(0.5, 0.5,
    height = 1,
    width = 1,
    gp = gpar(fill = "white", col = "black", lwd = 3)
  )
})

write_icon("geom_count", {
  textGrob(expression(Sigma), gp = gpar(cex = 4))
})

write_icon("geom_histogram", {
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

write_icon("geom_boxplot", {
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

write_icon("geom_crossbar", {
  gTree(children = gList(
    # crossbar
    rectGrob(
      0.3,
      0.6,
      width = 0.3,
      height = c(0.4, 0.4),
      vjust = 1
    ),
    segmentsGrob(c(0.15), c(0.5), c(0.45), c(0.5)),

    # error bar
    segmentsGrob(0.70, 0.5, 0.70, 0.90),
    segmentsGrob(0.55, 0.5, 0.85, 0.50),
    segmentsGrob(0.55, 0.9, 0.85, 0.90)
  ))
})

write_icon("geom_dotplot", {
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

write_icon("geom_freqpoly", {
  y <- c(0.2, 0.3, 0.5, 0.6, 0.2, 0.8, 0.5, 0.3)
  linesGrob(seq(0.1, 0.9, by = 0.1), y, gp = gpar(col = "grey20"))
})

write_icon("geom_path", {
  linesGrob(c(0.2, 0.4, 0.8, 0.6, 0.5), c(0.2, 0.7, 0.4, 0.1, 0.5))
})

write_icon("geom_contour", {
  gTree(children = gList(polygonGrob(
    c(0.45, 0.5, 0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)
  ),
    polygonGrob(
      c(0.25, 0.6, 0.8, 0.5), c(0.5, 0.2, 0.75, 0.9),
      gp = gpar(fill = NA)
    )))
})

write_icon("geom_hex", {
  theta <- seq(0, 2 * pi, length.out = 7)[-1]
  polygonGrob(
    0.5 + 0.4 * sin(theta),
    0.5 + 0.4 * cos(theta),
    gp = gpar(fill = "grey50", col = NA)
  )
})

write_icon("geom_line", {
  pos <- seq(0, 1, length.out = 5)
  linesGrob(pos, c(0.2, 0.7, 0.4, 0.8, 0.3), gp = gpar(lwd = 3))
})

write_icon("geom_step", {
  n <- 10
  xs <- rep(0:n, each = 2)[-2 * (n + 1)] / n
  ys <- c(0, rep(1:n, each = 2)) / n
  linesGrob(xs, ys, gp = gpar(col = "grey20", lwd = 3))
})

write_icon("geom_point", {
  pos <- seq(0.1, 0.9, length.out = 6)
  pointsGrob(
    x = pos,
    y = pos,
    pch = 19,
    gp = gpar(col = "black", cex = 0.5),
    default.units = "npc"
  )
})

write_icon("geom_jitter", {
  pointsGrob(
    x = c(0.25, 0.22, 0.34, 0.70, 0.77, 0.80),
    y = c(0.15, 0.24, 0.28, 0.65, 0.90, 0.75),
    pch = 19,
    gp = gpar(col = "black", cex = 0.5),
    default.units = "npc"
  )
})

write_icon("geom_pointrange", {
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

write_icon("geom_polygon", {
  polygonGrob(
    c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3),
    c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3),
    gp = gpar(fill = "grey20", col = NA)
  )
})

write_icon("geom_quantile", {
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

write_icon("geom_raster", {
  rectGrob(
    c(0.25, 0.25, 0.75, 0.75),
    c(0.25, 0.75, 0.75, 0.25),
    width = 0.5,
    height = c(0.67, 0.5, 0.67, 0.5),
    gp = gpar(col = "grey20", fill = c('#804070', '#668040'))
  )
})

write_icon("geom_rect", {
  rectGrob(
    c(0.3, 0.7),
    c(0.4, 0.8),
    height = c(0.4, 0.8),
    width = 0.3,
    vjust = 1,
    gp = gpar(fill = "grey20", col = NA)
  )
})

write_icon("geom_ribbon", {
  polygonGrob(
    c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
    c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
    gp = gpar(fill = "grey20", col = NA)
  )
})

write_icon("geom_spoke", {
  theta <- seq(0, 2 * pi, length.out = 10)[-1]
  r <- seq(0.1, 0.45, length.out = length(theta))
  segmentsGrob(
    0.5, 0.5,
    0.5 + sin(theta) * r,
    0.5 + cos(theta) * r,
    gp = gpar(col = "grey20")
  )
})

write_icon("geom_area", {
  polygonGrob(c(0, 0, 0.3, 0.5, 0.8, 1, 1),
    c(0, 1, 0.5, 0.6, 0.3, 0.8, 0),
    gp = gpar(fill = "grey20", col = NA))
})

write_icon("geom_density", {
  x <- seq(0, 1, length.out = 80)
  y <- dnorm(x, mean = 0.5, sd = 0.15)
  linesGrob(x, 0.05 + y / max(y) * 0.9, default.units = "npc")
})


write_icon("geom_rug", {
  x <- seq(0.15, 0.95, length.out = 8)
  gList(
    segmentsGrob(x, 0, x, 0 + 0.1, gp = gpar(lwd = 2)),
    segmentsGrob(0, x, 0 + 0.1, x, gp = gpar(lwd = 2))
  )
})

write_icon("geom_segment", {
  segmentsGrob(c(0.1, 0.3, 0.5, 0.7),
    c(0.3, 0.5, 0.1, 0.9),
    c(0.2, 0.5, 0.7, 0.9),
    c(0.8, 0.7, 0.4, 0.3))
})

write_icon("geom_smooth", {
  gTree(children = gList(polygonGrob(
    c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
    c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
    gp = gpar(fill = "grey60", col = NA)
  ),
    linesGrob(
      c(0, 0.3, 0.5, 0.8, 1), c(0.6, 0.4, 0.5, 0.4, 0.6)
    )))
})

write_icon("geom_text", {
  textGrob("text", rot = 45, gp = gpar(cex = 1.2))
})

write_icon("geom_tile", {
  rectGrob(
    c(0.25, 0.25, 0.75, 0.75),
    c(0.25, 0.75, 0.75, 0.25),
    width = 0.5,
    height = c(0.67, 0.5, 0.67, 0.5),
    gp = gpar(col = "grey20", fill = c('#804070', '#668040'))
  )
})

write_icon("geom_violin", {
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

# Position adjustments --------------------------------------------------------

write_icon("position_dodge", {
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

write_icon("position_fill", {
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

write_icon("position_identity", {
  rectGrob(
    0.5,
    c(0.5, 0.3),
    width = 0.4,
    height = c(0.5, 0.3),
    gp = gpar(col = "grey60", fill = c('#804070', '#668040')),
    vjust = 1
  )
})

file.copy("icons/geom_jitter.png", "icons/position_jitter.png")

write_icon("position_stack", {
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

write_icon("scale_alpha", {
  x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  rectGrob(x,
    width = 0.3,
    height = x,
    gp = gpar(fill = scales::alpha("black", x), col = NA))
})

write_icon("scale_colour_brewer", {
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = RColorBrewer::brewer.pal(5, "PuOr"), col = NA)
  )
})

write_icon("scale_colour_continuous", {
  g1 <- scale_fill_gradient()
  g1$train(1:5)

  g2 <- scale_fill_viridis_c()
  g2$train(1:5)

  x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  rectGrob(
    c(x, x),
    rep(c(0.25, 0.75), each = 5),
    width = 0.21,
    height = 0.5,
    gp = gpar(fill = c(g1$map(1:5), g2$map(5:1)), col = NA)
  )
})

write_icon("scale_colour_viridis_d", {
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = g$map(1:5), col = NA)
  )
})

write_icon("scale_colour_gradient", {
  g <- scale_fill_gradient()
  g$train(1:5)
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = g$map(1:5), col = NA)
  )
})

write_icon("scale_colour_viridis", {
  g <- scale_fill_viridis_c()
  g$train(1:5)
  rectGrob(
    c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = g$map(1:5), col = NA)
  )
})


write_icon("scale_colour_grey", {
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = gray(seq(0, 0.9, length.out = 5)), col = NA))
})

write_icon("scale_colour_hue", {
  rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9),
    width = 0.21,
    gp = gpar(fill = hcl(
      seq(0, 360, length.out = 6)[-6], c = 100, l = 65
    ), col = NA))
})

write_icon("scale_linetype", {
  gTree(children = gList(
    segmentsGrob(0, 0.25, 1, 0.25, gp = gpar(lty = 1, lwd = 3)),
    segmentsGrob(0, 0.50, 1, 0.50, gp = gpar(lty = 2, lwd = 3)),
    segmentsGrob(0, 0.75, 1, 0.75, gp = gpar(lty = 3, lwd = 3))
  ))
})

write_icon("scale_colour_manual", {
  textGrob("DIY", gp = gpar(cex = 1.2))
})

write_icon("scale_shape", {
  gp <- gpar(lwd = 3)
  gTree(children = gList(
    circleGrob(0.7, 0.7, r = 0.1, gp = gp),
    segmentsGrob(0.2, 0.3, 0.4, 0.3, gp = gp),
    segmentsGrob(0.3, 0.2, 0.3, 0.4, gp = gp),
    polygonGrob(c(0.2, 0.2, 0.4, 0.4), c(0.8, 0.6, 0.6, 0.8), gp = gp),
    polygonGrob(c(0.6, 0.7, 0.8), c(0.2, 0.4, 0.2), gp = gp)
  ))
})

write_icon("scale_size", {
  pos <- c(0.15, 0.3, 0.5, 0.75)
  circleGrob(pos,
    pos,
    r = (c(0.1, 0.2, 0.3, 0.4) / 2.5),
    gp = gpar(fill = "grey50", col = NA))
})

write_icon("scale_x_date", {
  textGrob("14/10/1979", gp = gpar(cex = 0.9), rot = 45)
})
