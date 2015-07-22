legend_point <- function(self, data, ...) {
  data <- aesdefaults(data, self$default_aes, list(...))

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

legend_abline <- function(self, data, ...) {
  data <- aesdefaults(data, self$default_aes, list(...))
  ggname(self$my_name(),
    segmentsGrob(0, 0, 1, 1, default.units = "npc",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        lwd = data$size * .pt,
        lty = data$linetype,
        lineend = "butt"
      )
    )
  )
}

legend_polygon <- function(self, data, ...)  {
  data <- aesdefaults(data, self$default_aes, list(...))

  with(data, grobTree(
    rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
    linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
  ))
}

legend_blank <- function(self, data, ...) {
  zeroGrob()
}

legend_boxplot <- function(self, data, ...)  {
  data <- aesdefaults(data, self$default_aes, list(...))
  gp <- with(data, gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt, lty = linetype))
  gTree(gp = gp, children = gList(
    linesGrob(0.5, c(0.1, 0.25)),
    linesGrob(0.5, c(0.75, 0.9)),
    rectGrob(height=0.5, width=0.75),
    linesGrob(c(0.125, 0.875), 0.5)
  ))
}

legend_crossbar <- function(self, data, ...)  {
  data <- aesdefaults(data, self$default_aes, list(...))
  gp <- with(data, gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt, lty = linetype))
  gTree(gp = gp, children = gList(
    rectGrob(height=0.5, width=0.75),
    linesGrob(c(0.125, 0.875), 0.5)
  ))
}

legend_path <- function(self, data, ...) {
  data$arrow <- NULL
  data <- aesdefaults(data, self$default_aes, list(...))

  with(data,
    ggname(self$my_name(), segmentsGrob(0.1, 0.5, 0.9, 0.5, default.units="npc",
    gp=gpar(col=alpha(colour, alpha), lwd=size * .pt,
      lty=linetype, lineend="butt")))
  )
}

legend_dotplot <- function(self, data, ...) {
  data$shape <- 21

  data <- aesdefaults(data, self$default_aes, list(...))

  with(data,
    pointsGrob(0.5, 0.5, size = unit(.5, "npc"), pch = shape,
      gp = gpar(
        col = alpha(colour, alpha),
        fill = alpha(fill, alpha))
    )
  )
}

legend_pointrange <- function(self, data, ...) {
  data <- aesdefaults(data, self$default_aes, list(...))

  grobTree(
    legend_path(self, data, ...),
    legend_point(self, transform(data, size = size * 4), ...)
  )
}

legend_smooth <- function(self, data, params, ...) {
  data <- aesdefaults(data, self$default_aes, list(...))
  data$fill <- alpha(data$fill, data$alpha)
  data$alpha <- 1

  if (is.null(params$se) || params$se) {
    gTree(children = gList(
      rectGrob(gp = gpar(col = NA, fill = data$fill)),
      legend_path(self, data, ...)
    ))
  } else {
    legend_path(self, data, ...)
  }
}

legend_text <- function(self, data, ...) {
  data <- aesdefaults(data, self$default_aes, list(...))
  textGrob(
    "a", 0.5, 0.5,
    rot = data$angle,
    gp = gpar(
      col = alpha(data$colour, data$alpha),
      fontsize = data$size * .pt
    )
  )
}

legend_vline <- function(self, data, ...) {
  data <- aesdefaults(data, self$default_aes, list(...))

  ggname(
    self$my_name(),
    segmentsGrob(0.5, 0, 0.5, 1, default.units = "npc",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        lwd = data$size * .pt,
        lty = data$linetype,
        lineend = "butt"
      )
    )
  )
}
