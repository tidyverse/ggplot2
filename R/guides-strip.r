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
labels_default <- function(plot, strip = plot$striplabel) {
  add.names <- function(x) {
    for(i in 1:ncol(x)) x[[i]] <- plot$strip.text(colnames(x)[i], x[,i])
    x
  }

  gm <- plot$facet$grid(plot$data)
  row.labels <- add.names(rrownames(gm))
  col.labels <- add.names(rcolnames(gm))
  
  strip_h <- apply(col.labels, c(2,1), ggstrip, strip.gp=plot$strip.gp, text.gp=plot$strip.text.gp)
  strip_v <- apply(row.labels, c(1,2), ggstrip, hor=FALSE, strip.gp=plot$strip.gp, text.gp=plot$strip.text.gp)

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
ggstrip <- function(text, horizontal=TRUE, strip.gp=ggopt()$strip.gp, text.gp=ggopt()$strip.text.gp) {
  if (is.list(text)) text <- text[[1]]
  ggname("strip", gTree(children = gList(
    ggname("background", rectGrob(gp=strip.gp)),
    ggname("label", textGrob(text, rot=-90 * (1 - horizontal), gp=text.gp)) 
  )))
}