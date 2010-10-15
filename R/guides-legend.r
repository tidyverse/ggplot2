# Legends
# Create and arrange legends for all scales.
# 
# This function gathers together all of the legends produced by 
# the scales that make up the plot and organises them into a 
# \code{\link[grid]{frameGrob}}.  
# 
# If there are no legends to create, this function will return \code{NULL}
# 
# @arguments scales object
# @arguments direction of scales, vertical by default
# @keyword hplot 
# @value frameGrob, or NULL if no legends
# @keyword internal
guide_legends_box <- function(scales, layers, default_mapping, horizontal = FALSE, theme) {

  # override alignment of legends box if theme$legend.box is specified
  if (!is.na(theme$legend.box)) {
    horizontal <- 1 == charmatch(theme$legend.box, c("horizontal","vertical"))
  }
  
  legs <- guide_legends(scales, layers, default_mapping, theme=theme)
  
  n <- length(legs)
  if (n == 0) return(zeroGrob())
  
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
# @arguments list of legend descriptions
# @arguments list description usage of aesthetics in geoms
# @keyword internal
# @value A list of grobs
# @alias build_legend
# @alias build_legend_data
#X theme_update(legend.background = theme_rect(size = 0.2))
#X qplot(mpg, wt, data = mtcars)
#X qplot(mpg, wt, data = mtcars, colour = cyl)
#X
#X # Legend with should expand to fit name
#X qplot(mpg, wt, data = mtcars, colour = factor(cyl))
#X 
#X qplot(mpg, wt, data = mtcars, colour = cyl) +
#X  opts(legend.position = c(0.5, 0.5), 
#X       legend.background = theme_rect(fill = "white", col = NA))
#X
#X mtcars$cyl2 <- factor(mtcars$cyl, 
#X   labels = c("a", "loooooooooooong", "two\nlines"))
#X qplot(mpg, wt, data = mtcars, colour = cyl2)
#X theme_set(theme_grey())
guide_legends <- function(scales, layers, default_mapping, theme) {
  legend <- scales$legend_desc(theme)
  if (length(legend$titles) == 0) return()
  
  hashes <- unique(legend$hash)
  lapply(hashes, function(hash) {
    keys <- legend$keys[legend$hash == hash]
    title <- legend$title[legend$hash == hash][[1]]
    
    if (length(keys) > 1) { 
      # Multiple scales for this legend      
      keys <- merge_recurse(keys, by = ".label")
    } else {
      keys <- keys[[1]]
    }
    build_legend(title, keys, layers, default_mapping, theme)
  })
}

build_legend <- function(name, mapping, layers, default_mapping, theme) {
 legend_data <- plyr::llply(layers, build_legend_data, mapping, default_mapping)

 direction <- charmatch(theme$legend.direction, c("vertical","horizontal"))

  # Calculate sizes for keys - mainly for v. large points and lines
  size_mat <- do.call("cbind", plyr::llply(legend_data, "[[", "size"))
  if (is.null(size_mat)) {
    key_sizes <- rep(0, nrow(mapping))
  } else {
    key_sizes <- apply(size_mat, 1, max)
  }

  # hjust for title of legend
  if (is.na(theme$legend.title.align)) {
    title <- theme_render(
      theme, "legend.title",
      name, x = 0, y = 0.5
    )
  } else {
    title <- theme_render(
      theme, "legend.title",
      name, hjust = theme$legend.title.align, x = theme$legend.title.align, y = 0.5
    )
  }

  # Compute heights and widths of legend table
  nkeys <- nrow(mapping)
  hgap <- vgap <- unit(0.3, "lines")

  if (is.na(theme$legend.text.align)) {
    numeric_labels <- all(sapply(mapping$.label, is.language)) || suppressWarnings(all(!is.na(sapply(mapping$.label, "as.numeric"))))
    hpos <- numeric_labels * 1    
  } else {
    hpos <- theme$legend.text.align
  }

  labels <- lapply(mapping$.label, function(label) {
    theme_render(theme, "legend.text", label, hjust = hpos, x = hpos, y = 0.5)
  })

  # direction == vertical (original)
  if(direction==1){
    label_width <- do.call("max", lapply(labels, grobWidth))
    label_width <- convertWidth(label_width, "cm")
    label_heights <- do.call("unit.c", lapply(labels, grobHeight))
    label_heights <- convertHeight(label_heights, "cm")

    width <- max(unlist(plyr::llply(legend_data, "[[", "size")), 0)
    key_width <- max(theme$legend.key.size, unit(width, "mm"))

    widths <- unit.c(
                     hgap, key_width,
                     hgap, label_width,
                     max(
                         unit(1, "grobwidth", title) - key_width - label_width,
                         hgap
                         )
                     )
    widths <- convertWidth(widths, "cm")

    heights <- unit.c(
                      vgap, 
                      unit(1, "grobheight", title),
                      vgap, 
                      unit.pmax(
                                theme$legend.key.size, 
                                label_heights, 
                                unit(key_sizes, "mm")
                                ),
                      vgap
                      )  
    heights <- convertHeight(heights, "cm")

  }else if(direction==2){
    label_width <- do.call("unit.c", lapply(labels, grobWidth))
    label_width <- convertWidth(label_width, "cm")
    label_heights <- do.call("max", lapply(labels, grobHeight))
    label_heights <- convertHeight(label_heights, "cm")

    height <- max(unlist(plyr::llply(legend_data, "[[", "size")), 0)
    key_heights <- max(theme$legend.key.size, unit(height, "mm"))

    key_width <- unit.pmax(theme$legend.key.size, unit(key_sizes, "mm"))
    # width of (key gap label gap) x nkeys
    kglg_width<-do.call("unit.c",lapply(1:length(key_width), function(i)unit.c(key_width[i], hgap, label_width[i], hgap)))
    widths <- unit.c(
                      hgap,
                      kglg_width,
                      max(
                          unit(0,"lines"),
                          unit.c(unit(1, "grobwidth", title) - (sum(kglg_width) - hgap))
                          )
                      )
    widths <- convertWidth(widths, "cm")

    heights <- unit.c(
                       vgap, 
                       unit(1, "grobheight", title),
                       vgap, 
                       max(
                           theme$legend.key.size,
                           label_heights, 
                           key_heights
                           ),
                       vgap
                       )  
  heights <- convertHeight(heights, "cm")

  }

  # horizontally center is pretty when direction is horizontal
  hjust = c("left","centre")[direction]
 
  # Layout the legend table
  legend.layout <- grid.layout(
    length(heights), length(widths), 
    widths = widths, heights = heights, 
    just = c(hjust, "centre")
  )

  fg <- ggname("legend", frameGrob(layout = legend.layout))
  fg <- placeGrob(fg, theme_render(theme, "legend.background"))

  fg <- placeGrob(fg, title, col = 2:(length(widths)-1), row = 2)
  for (i in 1:nkeys) {

    if(direction==1){
      fg <- placeGrob(fg, theme_render(theme, "legend.key"), col = 2, row = i+3)
    }else if(direction==2){
      fg <- placeGrob(fg, theme_render(theme, "legend.key"), col = 1+(i*4)-3, row = 4)
    }

    for(j in seq_along(layers)) {
      if (!is.null(legend_data[[j]])) {
        legend_geom <- Geom$find(layers[[j]]$geom$guide_geom())
        key <- legend_geom$draw_legend(legend_data[[j]][i, ],
           c(layers[[j]]$geom_params, layers[[j]]$stat_params))
        if(direction==1){
          fg <- placeGrob(fg, ggname("key", key), col = 2, row = i+3)
        }else if(direction==2){
          fg <- placeGrob(fg, ggname("key", key), col = 1+(i*4)-3, row = 4)
        }
      }
    }
    label <- theme_render(
      theme, "legend.text", 
      mapping$.label[[i]], hjust = hpos,
      x = hpos, y = 0.5
    )
    if(direction==1){
      fg <- placeGrob(fg, label, col = 4, row = i+3)
    }else if(direction==2){
      fg <- placeGrob(fg, label, col = 1+(i*4)-1, row = 4)
    }
  }
  fg
}

build_legend_data <- function(layer, mapping, default_mapping) {
  all <- names(c(layer$mapping, default_mapping, layer$stat$default_aes()))
  geom <- c(layer$geom$required_aes, names(layer$geom$default_aes()))
 
  matched <- intersect(intersect(all, geom), names(mapping))
  matched <- setdiff(matched, names(layer$geom_params))

  if (length(matched) > 0) {
    # This layer contributes to the legend
    if (is.na(layer$legend) || layer$legend) {
      # Default is to include it 
      layer$use_defaults(mapping[matched])
    } else {
      NULL
    }
  } else {
    # This layer does not contribute to the legend
    if (is.na(layer$legend) || !layer$legend) {
      # Default is to exclude it
      NULL
    } else {
      layer$use_defaults(NULL)[rep(1, nrow(mapping)), ]
    }
  }
}
