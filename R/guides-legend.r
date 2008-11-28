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
guide_legends_box <- function(scales, layers, default_mapping, horizontal = FALSE, theme) {
  legs <- guide_legends(scales, layers, default_mapping, theme=theme)
  
  n <- length(legs)
  if (n == 0) return(nullGrob())
  
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
guide_legends <- function(scales, layers, default_mapping, theme) {
  legends <- scales$legend_desc()
  if (length(legends) == 0) return()
  
  lapply(names(legends), function(var) {
    build_legend(var, legends[[var]], layers, default_mapping, theme)
  })
}

build_legend <- function(name, mapping, layers, default_mapping, theme) {
  
  legend_data <- llply(layers, function(layer) {
    used <- names(c(layer$mapping, default_mapping))
    matched <- intersect(used, names(mapping))
    if (length(matched) > 0) {
      layer$use_defaults(mapping[matched])
    } else {
      layer$use_defaults(NULL)[rep(1, nrow(mapping)), ]
    }
  })
  # if (length(legend_data) == 0) return(nullGrob())
  
  # Calculate sizes for keys - mainly for v. large points and lines
  size_mat <- do.call("cbind", llply(legend_data, "[[", "size"))
  widths <- size_mat[, laply(layers, function(l) l$geom$objname == "point")]
  width <- max(0, widths)
  heights <- apply(size_mat, 1, max)

  title <- theme_render(
    theme, "legend.title",
    name, x = 0, y = 0.5
  )
  
  nkeys <- nrow(mapping)
  hgap <- vgap <- unit(0.3, "lines")
  
  label.heights <- do.call("unit.c", 
    lapply(mapping$.label, function(x) stringHeight(as.expression(x))))
  label.widths  <- do.call("unit.c", 
    lapply(mapping$.label, function(x) stringWidth(as.expression(x))))

  widths <- unit.c(
    max(unit(1.4, "lines"), unit(width, "mm")), 
    hgap, 
    max(
      unit.c(unit(1, "grobwidth", title) - unit(1.4, "lines") - 2 * hgap),
      label.widths
    ),
    hgap
  )

  heights <- unit.c(
    unit(1, "grobheight", title) + 2 * vgap, 
    unit.pmax(unit(1.4, "lines"), vgap + label.heights, unit(heights, "mm"))
  )  

  # Layout the legend table
  legend.layout <- grid.layout(nkeys + 1, 4, 
    widths = widths, heights = heights, just=c("left","top"))
  fg <- ggname("legend", frameGrob(layout = legend.layout))
  fg <- placeGrob(fg, theme_render(theme, "legend.background"))

  numeric_labels <- all(sapply(mapping$.labels, is.language)) || suppressWarnings(all(!is.na(sapply(mapping$.labels, "as.numeric"))))
  hpos <- numeric_labels * 1

  fg <- placeGrob(fg, title, col=1:3, row=1)
  for (i in 1:nkeys) {
    
    fg <- placeGrob(fg, theme_render(theme, "legend.key"), col = 1, row = i+1)      
    for(j in seq_along(layers)) {
      legend_geom <- Geom$find(layers[[j]]$geom$guide_geom())
      key <- legend_geom$draw_legend(legend_data[[j]][i, ])
      fg <- placeGrob(fg, ggname("key", key), col = 1, row = i+1)      
    }
    label <- theme_render(
      theme, "legend.text", 
      mapping$.label[[i]], hjust = hpos,
      x = hpos, y = 0.5
    )
    fg <- placeGrob(fg, label, col = 3, row = i+1)
  }

  fg
}


# Build a legend grob
# Build the grob for a single legend.
# 
# @argument a single legend description
# @argument list description usage of aesthetics in geoms
# @value A grid grob
# @keyword internal
guide_legend <- function(legend, usage=usage, theme) {
  display <- legend$display
  display <- display[nrow(display):1, ]

  aesthetics <- setdiff(names(legend$display), "label")
  
  legend_f <- function(x) {
    geom <- Geom$find(x)
    used <- names(Filter(function(geom) any(geom == x), usage$aesthetic))
    params <- usage$parameters[[x]]
    
    function(data) geom$draw_legend(defaults(params, data[used]))
  }
  grobs <- lapply(unique(unlist(usage$aesthetic[aesthetics])), legend_f)

  title <- theme_render(
    theme, "legend.title",
    legend$name[[1]], x = 0, y = 0.5
  )
  
  nkeys <- nrow(display)
  hgap <- vgap <- unit(0.3, "lines")

  
  label.heights <- do.call("unit.c", lapply(display$label, function(x) stringHeight(as.expression(x))))
  label.widths  <- do.call("unit.c", lapply(display$label, function(x) stringWidth(as.expression(x))))

  grobwidth <- if ("point" %in% usage$mapping[aesthetics] && !is.null(display$size)) {
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
  fg <- placeGrob(fg, theme_render(theme, "legend.background"))

  numeric_labels <- all(sapply(display$label, is.language)) || suppressWarnings(all(!is.na(sapply(display$label, "as.numeric"))))
  hpos <- numeric_labels * 1

  fg <- placeGrob(fg, title, col=1:3, row=1)
  for (i in 1:nkeys) {
    df <- as.list(display[i,, drop=FALSE])
    
    fg <- placeGrob(fg, theme_render(theme, "legend.key"), col = 1, row = i+1)      
    for(grob in grobs) {
      fg <- placeGrob(fg, ggname("key", grob(df)), col = 1, row = i+1)      
    }
    label <- theme_render(
      theme, "legend.text", 
      display$label[[i]], hjust = hpos,
      x = hpos, y = 0.5
    )
    fg <- placeGrob(fg, label, col = 3, row = i+1)
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
    function(p) p$aesthetics_used(plot$mapping)
  )
  params <- lapply(plot$layers, function(p) p$geom_params)

  geom_names <- sapply(plot$layers, function(p) p$geom$guide_geom())
  names(aesthetics) <- geom_names
  names(params) <- geom_names
  
  list(
    aesthetics = lapply(invert(aesthetics), unique), 
    parameters = params
  )
}
