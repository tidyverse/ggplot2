# Legends
# Create and arrange legends for all scales.
# 
# This function gathers together all of the legends produced by 
# the scales that make up the plot and organises them into a 
# \\code{\\link[grid]{frameGrob}}.  
# 
# If there are no legends to create, this function will return \\code{NULL}
# 
# @arguments scales object
# @arguments direction of scales, vertical by default
# @keyword hplot 
# @value frameGrob, or NULL if no legends
# @keyword internal
guide_legends_box <- function(scales, scale_usage, horizontal = FALSE, background="grey90") {
  legs <- guide_legends(scales, scale_usage, background=background)
  
  n <- length(legs)
  if (n == 0) return()
  
  if (!horizontal) {
    width <-   do.call("max", lapply(legs, widthDetails))
    heights <- do.call("unit.c", lapply(legs, function(x) heightDetails(x) * 1.1))
    fg <- frameGrob(grid.layout(nrow=n, 1, widths=width, heights=heights, just="centre"), name="legends")
    for(i in 1:n) {
      fg <- placeGrob(fg, legs[[i]], row=i)
    }
  } else {
    height <- do.call("sum", lapply(legs, heightDetails))
    widths <- do.call("unit.c", lapply(legs, function(x) widthDetails(x) * 1.1))
    fg <- frameGrob(grid.layout(ncol=n, 1, widths=widths, heights=height, just="centre"), name="legends")
    for(i in 1:n) {
      fg <- placeGrob(fg, legs[[i]], col=i)
    }
  }
  fg
}

# Build all legend grob
# Build legends, merging where possible
# 
# @argument list of legend descriptions
# @argument list description usage of aesthetics in geoms
# @keyword internal
# @value A list of grobs
guide_legends <- function(scales, usage, background="grey80") {
  legends <- compact(lapply(scales$get_trained_scales(), function(sc) sc$legend_desc()))
  
  if (length(legends) == 0) 
    return()
  
  # Need to collapse legends describing same values into single data.frame
  # - first group by name
  legend_names <- unname(unlist(lapply(legends, "[", "name")))
  name_strings <- sapply(legend_names, deparse)
  names(legend_names) <- name_strings
  
  keys <- lapply(legends, "[[", "display")
  variables <- split(keys, name_strings)

  # - then merge data.frames
  keys_merged <- lapply(variables, merge_legends)
  legends_merged <- mapply(function(name, keys) list(name = legend_names[name], display=keys), names(keys_merged), keys_merged, SIMPLIFY = FALSE, USE.NAMES = FALSE)  
  
  lapply(legends_merged, guide_legend, usage=usage, background=background)
}

# Merge legends
# Merge multiple legend descriptions into one
# 
# Does not check that it makes sense to merge them.
# 
# @arguments list of legends to merge
# @keyword internal
merge_legends <- function(legends) {
  n <- length(legends)
  if (n < 2) return(legends[[1]])
  
  all <- legends[[1]]
  for(i in 2:n) 
    all <- merge(all, legends[[i]], by="label", sort="false")
  all
}

# Build a legend grob
# Build the grob for a single legend.
# 
# @argument a single legend description
# @argument list description usage of aesthetics in geoms
# @value A grid grob
# @keyword internal
guide_legend <- function(legend, usage=usage, background = "grey90") {
  display <- legend$display
  display <- display[nrow(display):1, ]

  aesthetics <- setdiff(names(legend$display), "label")
  
  legend_f <- function(x) {
    geom <- Geom$find(x)
    used <- names(Filter(function(geom) any(geom == x), usage))
    function(data) geom$draw_legend(data[used])
  }
  grobs <- lapply(unique(unlist(usage[aesthetics])), legend_f)

  title <- ggname("title", textGrob(legend$name[[1]], x = 0, y = 0.5, just = c("left", "centre"), 
    gp=gpar(fontface="bold")
  ))
  
  nkeys <- nrow(display)
  hgap <- vgap <- unit(0.3, "lines")

  
  label.heights <- do.call("unit.c", lapply(display$label, function(x) stringHeight(as.expression(x))))
  label.widths  <- do.call("unit.c", lapply(display$label, function(x) stringWidth(as.expression(x))))

  grobwidth <- if ("point" %in% usage[aesthetics] && !is.null(display$size)) {
    unit(max(display$size) / 2, "mm")
  } else {
    unit(0, "mm")
  }
  widths <- unit.c(
    unit(1.4, "lines"), 
    hgap, 
    max(
      unit.c(unit(1, "grobwidth", title) - unit(1.4, "lines") - 2 * hgap),
      label.widths,
      grobwidth 
    ),
    hgap
  )

  grobheight <- unit(nulldefault(display$size, 0), "mm")
  heights <- unit.c(
    unit(1, "grobheight", title) + 2 * vgap, 
    unit.pmax(unit(1.4, "lines"), vgap + label.heights, grobheight)
  )  

  # Layout the legend table
  legend.layout <- grid.layout(nkeys + 1, 4, widths = widths, heights = heights, just=c("left","top"))
  fg <- ggname("legend", frameGrob(layout = legend.layout))
  #fg <- placeGrob(fg, rectGrob(gp=gpar(fill="NA", col="NA", name="legend-background")))

  numeric_labels <- all(sapply(display$label, is.language)) || suppressWarnings(all(!is.na(sapply(display$label, "as.numeric"))))
  valign <- if(numeric_labels) "right" else "left"
  vpos   <- if(numeric_labels) 1 else 0

  fg <- placeGrob(fg, title, col=1:2, row=1)
  for (i in 1:nkeys) {
    df <- as.list(display[i,, drop=FALSE])
    fg <- placeGrob(fg, rectGrob(gp=gpar(col=background, fill=background)), col = 1, row = i+1)      
    for(grob in grobs) {
      fg <- placeGrob(fg, ggname("key", grob(df)), col = 1, row = i+1)      
    }
    fg <- placeGrob(fg, ggname("label", textGrob(display$label[[i]], x = vpos, y = 0.5, just = c(valign, "centre"))), col = 3, row = i+1)
  }

  fg
}

# Compute usage of scales
# Builds a list of aesthetics and the geoms that they are used by.
# 
# Used for drawing legends.
# 
# @arguments ggplot object
# @keyword internal
scale_usage <- function(plot) {
  aesthetics <- lapply(plot$layers, 
    function(p) p$aesthetics_used(plot$defaults)
  )
  params <- lapply(plot$layers, function(p) p$geom_params)

  geom_names <- sapply(plot$layers, function(p) p$geom$guide_geom())
  names(aesthetics) <- geom_names
  names(params) <- geom_names
  
  browser()
  lapply(invert(aesthetics), unique)
}
