# Produce a grob to be used as for panel backgrounds
# minor and major grid line positions are given as fractional positions and will
# be converted to `'native'` units by polylineGrob() downstream
#
# Any minor lines coinciding with major lines will be removed
guide_grid <- function(theme, panel_params, coord, square = TRUE) {

  x_major <- panel_params$x$mapped_breaks()
  x_minor <- setdiff(panel_params$x$mapped_breaks_minor(), x_major)

  y_major <- panel_params$y$mapped_breaks()
  y_minor <- setdiff(panel_params$y$mapped_breaks_minor(), y_major)

  transform <- if (isTRUE(square)) {
    if (inherits(coord, "CoordFlip")) {
      function(x) coord$transform(flip_axis_labels(x), panel_params)
    } else {
      function(x) coord$transform(x, panel_params)
    }
  } else {
    function(x) coord_munch(coord, x, panel_params)
  }

  grill <- Map(
    f = breaks_as_grid,
    var = list(y_minor, x_minor, y_major, x_major),
    type = c("minor.y", "minor.x", "major.y", "major.x"),
    MoreArgs = list(theme = theme, transform = transform)
  )
  grill <- compact(grill)

  background <- element_render(theme, "panel.background")
  if (!isTRUE(square) && !is.zero(background)) {
    gp <- background$gp
    background <- data_frame0(x = c(1, 1, -1, -1), y = c(1, -1, -1, 1)) * Inf
    background <- coord_munch(coord, background, panel_params, is_closed = TRUE)
    background <- polygonGrob(x = background$x, y = background$y, gp = gp)
  }

  ggname("grill", inject(grobTree(background, !!!grill)))
}

breaks_as_grid <- function(var, type, transform, theme) {
  n <- length(var)
  if (n < 1) {
    return(NULL)
  }
  df <- data_frame0(
    var   = rep(var, each = 2),
    alt   = rep(c(-Inf, Inf), n),
    group = rep(seq_along(var), each = 2)
  )
  colnames(df)[1:2] <-
    switch(type, major.y = , minor.y = c("y", "x"), c("x", "y"))
  df <- transform(df)
  element_render(
    theme, paste0("panel.grid.", type), x = df$x, y = df$y,
    id.lengths = vec_unrep(df$group)$times
  )
}
