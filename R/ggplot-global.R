# Environment that holds various global variables and settings for ggplot,
# such as the current theme. It is not exported and should not be directly
# manipulated by other packages.
ggplot_global <- new.env(parent = emptyenv())

# The current theme. Defined here only as placeholder, and defined properly
# in file "theme-current.R". This setup avoids circular dependencies among
# the various source files.
ggplot_global$theme_current <- list()

# Element tree for the theme elements. Defined here only as placeholder, and
# defined properly in file "theme-elements.r".
ggplot_global$element_tree <- list()

# List of all aesthetics known to ggplot
# (In the future, .all_aesthetics should be removed in favor
# of direct assignment to ggplot_global$all_aesthetics, see below.)
.all_aesthetics <- c(
  "adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
  "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
  "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
  "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z",
  "intercept"
)

ggplot_global$all_aesthetics <- .all_aesthetics

# Aesthetic aliases
# (In the future, .base_to_ggplot should be removed in favor
# of direct assignment to ggplot_global$base_to_ggplot, see below.)
.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)

ggplot_global$base_to_ggplot <- .base_to_ggplot

ggplot_global$x_aes <- c("x", "xmin", "xmax", "xend", "xintercept",
  "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")

ggplot_global$y_aes <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
  "ymax_final", "lower", "middle", "upper", "y0", "intercept")
