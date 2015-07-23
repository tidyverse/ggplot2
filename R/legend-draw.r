legend_point <- function(self, data, params) {
  pointsGrob(
    0.5, 0.5,
    pch = data$shape,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fill = alpha(data$fill, data$alpha),
      fontsize = data$size * .pt + data$stroke * .stroke / 2,
      lwd = data$stroke * .stroke / 2
    )
  )
}

legend_abline <- function(self, data, params) {
  segmentsGrob(0, 0, 1, 1, default.units = "npc",
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )
}

legend_polygon <- function(self, data, params) {
  grobTree(
    rectGrob(gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype
    )),
    linesGrob(gp = gpar(
        col = data$colour,
        lwd = data$size * .pt,
        lineend = "butt",
        lty = data$linetype
    ))
  )
}

legend_blank <- function(self, data, params) {
  zeroGrob()
}

legend_boxplot <- function(self, data, params) {
  grobTree(
    linesGrob(0.5, c(0.1, 0.25)),
    linesGrob(0.5, c(0.75, 0.9)),
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}

legend_crossbar <- function(self, data, params) {
  grobTree(
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}

legend_path <- function(self, data, params) {
  segmentsGrob(0.1, 0.5, 0.9, 0.5, default.units = "npc",
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    ),
    arrow = params$arrow
  )
}

legend_dotplot <- function(self, data, params) {
  data$shape <- 21

  pointsGrob(0.5, 0.5, size = unit(.5, "npc"), pch = data$shape,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fill = alpha(data$fill, data$alpha)
    )
  )
}

legend_pointrange <- function(self, data, params) {
  grobTree(
    legend_path(self, data, params),
    legend_point(self, transform(data, size = data$size * 4), params)
  )
}

legend_smooth <- function(self, data, params) {
  data$fill <- alpha(data$fill, data$alpha)
  data$alpha <- 1

  if (is.null(params$se) || params$se) {
    grobTree(
      rectGrob(gp = gpar(col = NA, fill = data$fill)),
      legend_path(self, data, params)
    )
  } else {
    legend_path(self, data, params)
  }
}

legend_text <- function(self, data, params) {
  textGrob(
    "a", 0.5, 0.5,
    rot = data$angle,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fontsize = data$size * .pt
    )
  )
}

legend_vline <- function(self, data, params) {
  segmentsGrob(0.5, 0, 0.5, 1, default.units = "npc",
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )
}
