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
labels_default <- function(gm, theme) {
  add.names <- function(x) {
    for(i in 1:ncol(x)) x[[i]] <- theme$strip.label(colnames(x)[i], x[,i])
    x
  }

  row.labels <- add.names(rrownames(gm))
  col.labels <- add.names(rcolnames(gm))
  
  strip_h <- apply(col.labels, c(2,1), ggstrip, theme = theme)
  # if (nrow(strip_h) == 1 && ncol(strip_h) == 1) strip_h <- matrix(list(nullGrob()))
  strip_v <- apply(row.labels, c(1,2), ggstrip, horizontal=FALSE, theme=theme)
  # if (nrow(strip_v) == 1 && ncol(strip_v) == 1) strip_v <- matrix(list(nullGrob()))

  list(
    h = strip_h, 
    v = strip_v
  )
}


# Grob strip
# Grob for strip labels
# 
# @arguments text to display
# @arguments orientation, horizontal or vertical
# @keyword hplot 
# @keyword internal
ggstrip <- function(text, horizontal=TRUE, theme) {
  text_theme <- if (horizontal) "strip.text.x" else "strip.text.y"
  if (is.list(text)) text <- text[[1]]

  label <- theme_render(theme, text_theme, text)

  ggname("strip", absoluteGrob(
    grobTree(
      theme_render(theme, "strip.background"),
      label
    ),
    width = grobWidth(label) + unit(0.5, "lines"),
    height = grobHeight(label) + unit(0.5, "lines")
  ))
}