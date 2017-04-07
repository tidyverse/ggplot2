# Produce a grob to be used as for panel backgrounds
guide_grid <- function(theme, x.minor, x.major, y.minor, y.major) {

  x.minor <- setdiff(x.minor, x.major)
  y.minor <- setdiff(y.minor, y.major)

  ggname("grill", grobTree(
    element_render(theme, "panel.background"),
    if (length(y.minor) > 0) element_render(
      theme, "panel.grid.minor.y",
      x = rep(0:1, length(y.minor)), y = rep(y.minor, each = 2),
      id.lengths = rep(2, length(y.minor))
    ),
    if (length(x.minor) > 0) element_render(
      theme, "panel.grid.minor.x",
      x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)),
      id.lengths = rep(2, length(x.minor))
    ),
    if (length(y.major) > 0) element_render(
      theme, "panel.grid.major.y",
      x = rep(0:1, length(y.major)), y = rep(y.major, each = 2),
      id.lengths = rep(2, length(y.major))
    ),
    if (length(x.major) > 0) element_render(
      theme, "panel.grid.major.x",
      x = rep(x.major, each = 2), y = rep(0:1, length(x.major)),
      id.lengths = rep(2, length(x.major))
    )
  ))
}
