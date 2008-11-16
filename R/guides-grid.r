guide_grid <- function(theme, x.minor, x.major, y.minor, y.major) {
  ggname("grill", grobTree(
    theme_render(theme, "panel.background"),
    
    theme_render(
      theme, "panel.grid.minor", name = "y",
      x = rep(0:1, length(y.minor)), y = rep(y.minor, each=2), 
      id.lengths = rep(2, length(y.minor))
    ),
    theme_render(
      theme, "panel.grid.minor", name = "x", 
      x = rep(x.minor, each=2), y = rep(0:1, length(x.minor)),
      id.lengths = rep(2, length(x.minor))
    ),

    theme_render(
      theme, "panel.grid.major", name = "y",
      x = rep(0:1, length(y.major)), y = rep(y.major, each=2), 
      id.lengths = rep(2, length(y.major))
    ),
    theme_render(
      theme, "panel.grid.major", name = "x", 
      x = rep(x.major, each=2), y = rep(0:1, length(x.major)), 
      id.lengths = rep(2, length(x.major))
    )
  ))
}