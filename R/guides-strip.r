# Default labels
# Generate default facet labels.
# 
# Facet labels are only displayed when there are facets in a particular
# direction.  By default the labels consist of the variable name : value.
# You can't currently change this display. but it will be an option in the near
# future.
#
# @arguments plot object
# @value gList containg text grobs with appropriate viewports
# @keyword hplot
# @keyword internal
labels_default <- function(plot, theme) {
  add.names <- function(x) {
    for(i in 1:ncol(x)) x[[i]] <- theme$strip.label(colnames(x)[i], x[,i])
    x
  }

  gm <- plot$facet$grid(plot$data)
  row.labels <- add.names(rrownames(gm))
  col.labels <- add.names(rcolnames(gm))
  
  strip_h <- apply(col.labels, c(2,1), ggstrip, theme = theme)
  strip_v <- apply(row.labels, c(1,2), ggstrip, horizontal=FALSE, theme=theme)

  labels_grobs <- unlist(compact(list(
    if (ncol(gm) > 1) plot_grob_matrix(strip_h),
    if (nrow(gm) > 1) plot_grob_matrix(strip_v)
  )), recursive=FALSE)
  
  if (!is.null(labels_grobs)) do.call("gList", labels_grobs)
}


# Grob strip
# Grob for strip labels
# 
# @arguments text to display
# @arguments orientation, horizontal or vertical
# @keyword hplot 
# @keyword internal
ggstrip <- function(text, horizontal=TRUE, theme) {
  text_theme <- if (horizontal) "strip.title.x" else "strip.title.y"
  
  if (is.list(text)) text <- text[[1]]
  ggname("strip", grobTree(
    theme_render(theme, "strip.background"),
    theme_render(theme, text_theme, text)
  ))
}