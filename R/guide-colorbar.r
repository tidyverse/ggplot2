## colorbar guide
##
## TODO: optimize justification of lable and title automatically based on the position and direction
## TODO: refactor horizontal and vertical grob gen. Is it better to separate out?
## TODO: documentation. but after the design and implementation is fixed.
guide_colorbar <- function(
                           
  ##　title
  title = NULL,
  title.position = NULL,
  title.angle = NULL,
  title.hjust = NULL,
  title.vjust = NULL,
  title.theme = NULL,

  ## label
  label = TRUE,
  label.position = NULL,
  label.angle = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
  label.theme = NULL,

  ## bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 20,
  raster = TRUE,

  ## ticks
  ticks = TRUE,
  nodraw.ul = FALSE,
  nodraw.ll = FALSE,

  ## general
  direction = NULL,
  default.unit = "line",
                           
  ...) {
  
  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list(
    ##　title
    title = title,
    title.position = title.position,
    title.angle = title.angle,
    title.hjust = title.hjust,
    title.vjust = title.vjust,
    title.theme = title.theme,

    ## label
    label = label,
    label.position = label.position,
    label.angle = label.angle,
    label.hjust = label.hjust,
    label.vjust = label.vjust,
    label.theme = label.theme,

    ## bar
    barwidth = barwidth,
    barheight = barheight,
    nbin = nbin,
    raster = raster,

    ## ticks
    ticks = ticks,
    nodraw.ul = nodraw.ul,
    nodraw.ll = nodraw.ll,

    ## general
    direction = direction,
    default.unit = default.unit,
                 
    ..., name="colorbar"),
    class=c("guide", "colorbar"))
}

guide_train.colorbar <- function(guide, scale) {

  ## do nothing if scale are inappropriate
  if (length(intersect(scale$aesthetics, c("color", "colour", "fill"))) == 0) {
    warning("colorbar guide needs colour or fill scales.")
    return(NULL)
  }
  if (!inherits(scale, "continuous")) {
    warning("colorbar guide needs continuous scales.")
    return(NULL)
  }
  
  
  ## ticks - label (i.e. breaks)
  output <- scale$aesthetics[1]
  breaks <- scale_breaks(scale)
  guide$key <- data.frame(scale_map(scale, breaks), I(scale_labels(scale, breaks)), breaks,
                          stringsAsFactors = FALSE)
  
  ## .value = breaks (numeric) is used for determining the position of ticks in gengrob
  names(guide$key) <- c(output, ".label", ".value")

  ## bar specification (number of divs etc)
  .bar <- discard(pretty(scale_limits(scale), n = guide$nbin), scale_limits(scale))
  guide$bar <- data.frame(colour=scale_map(scale, .bar), value=.bar, stringsAsFactors = FALSE)
  guide$hash <- with(guide, digest(list(title, key$.label, bar, name)))
  guide
}

## simply discards the new guide
guide_merge.colorbar <- function(guide, new_guide) {
  guide
}

## this guide is not geom-based.
guide_geom.colorbar <- function(guide, ...) {
  guide
}

guide_gengrob.colorbar <- function(guide, theme) {
  if (guide$direction == "horizontal") grob <- guide_gengrob_colorbar.horizontal(guide, theme)
  else if (guide$direction == "vertical") grob <- guide_gengrob_colorbar.vertical(guide, theme)
  grob
}

guide_gengrob_colorbar.horizontal <- function(guide, theme) {

  ## default setting
  label.position <- guide$label.position %||% "bottom"
  if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")
  
  title.position <- guide$title.position %||% "left"
  if (!title.position %in% c("top", "bottom", "left", "right")) stop("title position \"", title.position, "\" is invalid")
  
  barwidth <- convertWidth(guide$barwidth %||% (theme$legend.key.width * 5), "mm")
  barheight <- convertHeight(guide$barheight %||% theme$legend.key.height, "mm")
  
  barwidth.c <- c(barwidth)
  barheight.c <- c(barheight)
  
  nbreak <- nrow(guide$key)

  ## gap between keys etc
  hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  if (guide$raster)
    grob.bar <- rasterGrob(image = t(guide$bar$colour), width=barwidth.c, height=barheight.c, default.units = "mm", gp=gpar(col=NA), interpolate = TRUE)

  ## tick and label position
  tic_pos.c <- rescale(guide$key$.value, c(0.5, guide$nbin-0.5), range(guide$bar$value)) * barwidth.c / guide$nbin
  label_pos <- unit(tic_pos.c, "mm")
  if (guide$nodraw.ul) tic_pos.c <- tic_pos.c[-1]
  if (guide$nodraw.ll) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]

  ## title
  ## hjust of title should depend on title.position
  grob.title <- {
    g <-
      if (is.logical(guide$title) && guide$title == FALSE) zeroGrob()
      else if(!is.null(guide$title.theme)) guide$title.theme(label=guide$title, x = 0.5, y = 0.5, name = grobName(NULL, "guide.title"))
      else theme_render(theme, "legend.title", guide$title, hjust = 1, x = 1, y = 0.5)
    if (!is.null(guide$title.angle)) g <- editGrob(g, rot = guide$title.angle)
    if (!is.null(guide$title.hjust)) g <- editGrob(g, hjust = guide$title.hjust)
    if (!is.null(guide$title.vjust)) g <- editGrob(g, vjust = guide$title.vjust)
    g
  }
  
  title_width <- convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  ## label
  grob.label <- {
    g <- 
      if (!guide$label) zeroGrob()
      else if(!is.null(guide$label.theme)) guide$label.theme(label=guide$key$.label, x = label_pos, y = 0.5, name = grobName(NULL, "guide.label"))
      else theme_render(theme, "legend.text", guide$key$.label, x = label_pos, y = 0.5)
    if (!is.null(guide$label.angle)) g <- editGrob(g, rot = guide$label.angle)
    if (!is.null(guide$label.hjust)) g <- editGrob(g, hjust = guide$label.hjust)
    if (!is.null(guide$label.vjust)) g <- editGrob(g, vjust = guide$label.vjust)
    g
  }
  
  label_width <- convertWidth(grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- convertHeight(grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)

  ## ticks
  grob.ticks <-
    if (!guide$ticks) zeroGrob()
    else segmentsGrob(
                      x0 = rep(tic_pos.c, 2),
                      y0 = c(rep(0, nbreak), rep(barheight.c * (4/5), nbreak)),
                      x1 = rep(tic_pos.c, 2),
                      y1 = c(rep(barheight.c * (1/5), nbreak), rep(barheight.c, nbreak)),
                      default.units = "mm",                                      
                      gp = gpar(col="white", lwd=0.5, lineend="butt"),
                      )

  ## horizontal
  switch(label.position,
    "top" = {
      bl_widths <- barwidth.c
      bl_heights <- c(label_height.c, vgap, barheight.c)
      vps <- list(bar.row = 3, bar.col = 1,
                  label.row = 1, label.col = 1)
    },
    "bottom" = {
      bl_widths <- barwidth.c
      bl_heights <- c(barheight.c, vgap, label_height.c)
      vps <- list(bar.row = 1, bar.col = 1,
                  label.row = 3, label.col = 1)
    })

  switch(title.position,
    "top" = {
      widths <- c(hgap, bl_widths, max(hgap, title_width.c-sum(bl_widths)))
      heights <- c(vgap, title_height.c, vgap, bl_heights, vgap)
      vps <- with(vps,
                  list(bar.row = bar.row+3, bar.col = bar.col+1,
                       label.row = label.row+3, label.col = label.col+1,
                       title.row = 2, title.col = 2:3))
    },
    "bottom" = {
      widths <- c(hgap, bl_widths, max(hgap, title_width.c-sum(bl_widths)))
      heights <- c(vgap, bl_heights, vgap, title_height.c, vgap)
      vps <- with(vps, 
                  list(bar.row = bar.row+1, bar.col = bar.col+1,
                       label.row = label.row+1, label.col = label.col+1,
                       title.row = length(heights)-1, title.col = 2:3))
    },
    "left" = {
      widths <- c(hgap, title_width.c, hgap, bl_widths, hgap)
      heights <- c(vgap, bl_heights, max(vgap, title_height.c-sum(bl_heights)))
      vps <- with(vps, 
                  list(bar.row = bar.row+1, bar.col = bar.col+3,
                       label.row = label.row+1, label.col = label.col+3,
                       title.row = 2:3, title.col = 2))
    },
    "right" = {
      widths <- c(hgap, bl_widths, hgap, title_width.c, hgap)
      heights <- c(vgap, bl_heights, max(vgap, title_height.c-sum(bl_heights)))
      vps <- with(vps, 
                  list(bar.row = bar.row+1, bar.col = bar.col+1,
                       label.row = label.row+1, label.col = label.col+1,
                       title.row = 2:3, title.col = length(widths)-1))
    })

  legend.layout <- grid.layout(length(heights), length(widths), 
                               widths = unit(widths, "mm"), heights = unit(heights, "mm"), 
                               just = "left")
  
  ## background
  grob.background <- theme_render(theme, "legend.background")

  ## set viewpoint
  grob.title$vp <- viewport(layout.pos.row = vps$title.row, layout.pos.col = vps$title.col)
  grob.bar$vp <- viewport(layout.pos.row = vps$bar.row, layout.pos.col = vps$bar.col)
  grob.label$vp <- viewport(layout.pos.row = vps$label.row, layout.pos.col = vps$label.col)
  grob.ticks$vp <- grob.bar$vp

  gTree(children = gList(grob.background, grob.title, grob.bar, grob.label, grob.ticks), vp = viewport(layout=legend.layout))
}

guide_gengrob_colorbar.vertical <- function(guide, theme) {

  ## default setting
  label.position <- guide$label.position %||% "right"
  if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")
  
  title.position <- guide$title.position %||% "top"
  if (!title.position %in% c("top", "bottom", "left", "right")) stop("title position \"", title.position, "\" is invalid")
  
  barwidth <- convertWidth(guide$barwidth %||% theme$legend.key.width, "mm")
  barheight <- convertHeight(guide$barheight %||% (theme$legend.key.height * 5), "mm")
  
  barwidth.c <- c(barwidth)
  barheight.c <- c(barheight)
  
  nbreak <- nrow(guide$key)
  
  ## gap between keys etc
  hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap
  
  if (guide$raster)
    grob.bar <- rasterGrob(image = guide$bar$colour, width=barwidth.c, height=barheight.c, default.units = "mm", gp=gpar(col=NA), interpolate = TRUE)

  ## tick and label position
  tic_pos.c <- rescale(guide$key$.value, c(0.5, guide$nbin-0.5), range(guide$bar$value)) * barheight.c / guide$nbin
  label_pos <- unit(tic_pos.c, "mm")
  if (guide$nodraw.ul) tic_pos.c <- tic_pos.c[-1]
  if (guide$nodraw.ll) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]

  ## title
  ## hjust of title should depend on title.position
  grob.title <- {
    g <-
      if (is.logical(guide$title) && guide$title == FALSE) zeroGrob()
      else if(!is.null(guide$title.theme)) guide$title.theme(label=guide$title, x = 0.5, y = 0.5, name = grobName(NULL, "guide.title"))
      else theme_render(theme, "legend.title", guide$title, hjust = 1, x = 1, y = 0.5)
    if (!is.null(guide$title.angle)) g <- editGrob(g, rot = guide$title.angle)
    if (!is.null(guide$title.hjust)) g <- editGrob(g, hjust = guide$title.hjust)
    if (!is.null(guide$title.vjust)) g <- editGrob(g, vjust = guide$title.vjust)
    g
  }

  title_width <- convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  ## label
  grob.label <- {
    g <- 
      if (!guide$label) zeroGrob()
      else if(!is.null(guide$label.theme)) guide$label.theme(label=guide$key$.label, x = 0.5, y = label_pos, name = grobName(NULL, "guide.label"))
      else theme_render(theme, "legend.text", guide$key$.label, x = 0.5, y = label_pos)
    if (!is.null(guide$label.angle)) g <- editGrob(g, rot = guide$label.angle)
    if (!is.null(guide$label.hjust)) g <- editGrob(g, hjust = guide$label.hjust)
    if (!is.null(guide$label.vjust)) g <- editGrob(g, vjust = guide$label.vjust)
    g
  }

  label_width <- convertWidth(grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- convertHeight(grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)

  ## ticks
  grob.ticks <-
    if (!guide$ticks) zeroGrob()
    else segmentsGrob(
                      x0 = c(rep(0, nbreak), rep(barwidth.c * (4/5), nbreak)),
                      y0 = rep(tic_pos.c, 2),
                      x1 = c(rep(barwidth.c * (1/5), nbreak), rep(barwidth.c, nbreak)),
                      y1 = rep(tic_pos.c, 2),
                      default.units = "mm",                                      
                      gp = gpar(col="white", lwd=0.5, lineend="butt")
                      )

  ## horizontal
  switch(label.position,
    "left" = {
      bl_widths <- c(label_width.c, vgap, barwidth.c)
      bl_heights <- barheight.c
      vps <- list(bar.row = 1, bar.col = 3,
                  label.row = 1, label.col = 1)
    },
    "right" = {
      bl_widths <- c(barwidth.c, vgap, label_width.c)
      bl_heights <- barheight.c
      vps <- list(bar.row = 1, bar.col = 1,
                  label.row = 1, label.col = 3)
    })

  switch(title.position,
    "top" = {
      widths <- c(hgap, bl_widths, max(hgap, title_width.c-sum(bl_widths)))
      heights <- c(vgap, title_height.c, vgap, bl_heights, vgap)
      vps <- with(vps,
                  list(bar.row = bar.row+3, bar.col = bar.col+1,
                       label.row = label.row+3, label.col = label.col+1,
                       title.row = 2, title.col = 2:3))
    },
    "bottom" = {
      widths <- c(hgap, bl_widths, max(hgap, title_width.c-sum(bl_widths)))
      heights <- c(vgap, bl_heights, vgap, title_height.c, vgap)
      vps <- with(vps, 
                  list(bar.row = bar.row+1, bar.col = bar.col+1,
                       label.row = label.row+1, label.col = label.col+1,
                       title.row = length(heights)-1, title.col = 2:3))
    },
    "left" = {
      widths <- c(hgap, title_width.c, hgap, bl_widths, hgap)
      heights <- c(vgap, bl_heights, max(vgap, title_height.c-sum(bl_heights)))
      vps <- with(vps, 
                  list(bar.row = bar.row+1, bar.col = bar.col+3,
                       label.row = label.row+1, label.col = label.col+3,
                       title.row = 2:3, title.col = 2))
    },
    "right" = {
      widths <- c(hgap, bl_widths, hgap, title_width.c, hgap)
      heights <- c(vgap, bl_heights, max(vgap, title_height.c-sum(bl_heights)))
      vps <- with(vps, 
                  list(bar.row = bar.row+1, bar.col = bar.col+1,
                       label.row = label.row+1, label.col = label.col+1,
                       title.row = 2:3, title.col = length(widths)-1))
    })

  legend.layout <- grid.layout(length(heights), length(widths), 
                               widths = unit(widths, "mm"), heights = unit(heights, "mm"), 
                               just = "left")
  
  ## background
  grob.background <- theme_render(theme, "legend.background")

  ## set viewpoint
  grob.title$vp <- viewport(layout.pos.row = vps$title.row, layout.pos.col = vps$title.col)
  grob.bar$vp <- viewport(layout.pos.row = vps$bar.row, layout.pos.col = vps$bar.col)
  grob.label$vp <- viewport(layout.pos.row = vps$label.row, layout.pos.col = vps$label.col)
  grob.ticks$vp <- grob.bar$vp

  gTree(children = gList(grob.background, grob.title, grob.bar, grob.label, grob.ticks), vp = viewport(layout=legend.layout))
}
