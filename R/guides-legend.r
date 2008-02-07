gglegends <- function(legends, usage) {
  # Need to collapse legends describing same values into single data.frame
  # - first group by name
  names <- unname(unlist(lapply(legends, "[", "name")))
  keys <- lapply(legends, "[[", "display")
  variables <- split(keys, names)

  # - then merge data.frames
  keys_merged <- unname(lapply(variables, merge_legends))
  legends_merged <- mapply(function(name, keys) list(name = name, display=keys), unique(names), keys_merged, SIMPLIFY = FALSE, USE.NAMES = FALSE)  
  
  lapply(legends_merged, gglegend, usage=usage)
}

merge_legends <- function(legends) {
  n <- length(legends)
  if (n < 2) return(legends[[1]])
  
  all <- legends[[1]]
  for(i in 2:n) 
    all <- merge(all, legends[[i]], by="label", sort="false")
  all
}

gglegend <- function(legend, usage=usage) {
  display <- legend$display

  aesthetics <- setdiff(names(legend$display), "label")
  
  legend_f <- function(x) {
    geom <- Geom$find(x)
    function(data) geom$draw_legend(data)
  }
  grobs <- lapply(unique(unlist(usage[aesthetics])), legend_f)

  title <- ggname("title", textGrob(legend$name, x = 0, y = 0.5, just = c("left", "centre"), 
    gp=gpar(fontface="bold")
  ))
  
  nkeys <- nrow(display)
  hgap <- vgap <- unit(0.3, "lines")
  
  label.heights <- do.call("unit.c", lapply(display$label, function(x) stringHeight(as.expression(x))))
  label.widths  <- do.call("unit.c", lapply(display$label, function(x) stringWidth(as.expression(x))))

  widths <- unit.c(
    unit(1.4, "lines"), 
    hgap, 
    max(unit.c(unit(1, "grobwidth", title) - unit(1.4, "lines") - 2 * hgap), label.widths),
    hgap
  )

  heights <- unit.c(
    unit(1, "grobheight", title) + 2 * vgap, 
    unit.pmax(unit(1.4, "lines"), vgap + label.heights)
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
    for(grob in grobs) {
      fg <- placeGrob(fg, ggname("key", grob(df)), col = 1, row = i+1)      
    }
    fg <- placeGrob(fg, ggname("label", textGrob(display$label[[i]], x = vpos, y = 0.5, just = c(valign, "centre"))), col = 3, row = i+1)
  }

  fg
}