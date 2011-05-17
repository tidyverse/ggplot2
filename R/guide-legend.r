## legend guide
##
## TODO: optimize justification of lable and title automatically based on the position and direction
## TODO: refactor horizontal and vertical grob gen. Is it better to separate out?
## TODO: documentation. but after the design and implementation is fixed.

guide_legend <- function(
                         
  ##　title
  title = waiver(),
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

  ## key
  keywidth = NULL,
  keyheight = NULL,

  ## general
  direction = NULL,
  default.unit = "line",
  set.aes = list(),                       
                         
  ...) {
  
  if (!is.null(keywidth) && !is.unit(keywidth)) keywidth <- unit(keywidth, default.unit)
  if (!is.null(keyheight) && !is.unit(keyheight)) keyheight <- unit(keyheight, default.unit)
  
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
    keywidth = keywidth,
    keyheight = keyheight,

    ## general
    direction = direction,
    default.unit = default.unit,
    set.aes = set.aes,

    ..., name="legend"),
    class=c("guide", "legend"))
}

guide_train.legend <- function(guide, scale) {
  guide$key <- data.frame(
                     scale_map(scale, scale_breaks(scale)), I(scale_labels(scale)), 
                     stringsAsFactors = FALSE)
  names(guide$key) <- c(scale$aesthetics[1], ".label")
#  for (aes in names(guide$set.aes)) guide$key[[aes]] <- guide$set.aes[[aes]]
  guide$hash <- with(guide, digest(list(title, key$.label, direction, name)))
  guide
}

guide_merge.legend <- function(guide, new_guide) {
  guide$key <- merge(guide$key, new_guide$key)
  guide$set.aes <- c(guide$set.aes, new_guide$set.aes)
  if (any(duplicated(names(guide$set.aes)))) warning("Duplicated set.aes is ignored.")
  guide$set.aes <- guide$set.aes[!duplicated(names(guide$set.aes))]
  guide
}

guide_geom.legend <- function(guide, layers, default_mapping) {

  ## TODO: how to deal with same geoms of multiple layers.
  ##
  ## currently all geoms are overlayed irrespective to that they are duplicated or not.
  ## but probably it is better to sensitive to that and generate only one geom like this:
  ##
  ## geoms <- unique(sapply(layers, function(layer) if (is.na(layer$legend) || layer$legend) layer$geom$guide_geom() else NULL))
  ## 
  ## but in this case, some conflicts occurs, e.g.,
  ##
  ## d <- data.frame(x=1:5, y=1:5, v=factor(1:5))
  ## ggplot(d, aes(x, y, colour=v, group=1)) + geom_point() + geom_line(colour="red", legend=T) + geom_rug(colour="blue", legend=T)
  ##
  ## geom_line generate path geom with red and geom_rug generate it with blue.
  ## how to deal with them ?
  
  ## arrange common data for vertical and horizontal guide
  guide$geoms <- llply(layers, function(layer) {

    all <- names(c(layer$mapping, default_mapping, layer$stat$default_aes()))
    geom <- c(layer$geom$required_aes, names(layer$geom$default_aes()))
    matched <- intersect(intersect(all, geom), names(guide$key))
    matched <- setdiff(matched, names(layer$geom_params))
    data <- 
      if (length(matched) > 0) {
        ## This layer contributes to the legend
        if (is.na(layer$legend) || layer$legend) {
          ## Default is to include it 
          layer$use_defaults(guide$key[matched])
        } else {
          NULL
        }
      } else {
        ## This layer does not contribute to the legend
        if (is.na(layer$legend) || !layer$legend) {
          ## Default is to exclude it
          NULL
        } else {
          layer$use_defaults(NULL)[rep(1, nrow(guide$key)), ]
        }
      }
    if (is.null(data)) return(NULL)
    
    ## set.aes in guide_legend manually changes the geom
    for (aes in intersect(names(guide$set.aes), names(data))) data[[aes]] <- guide$set.aes[[aes]]

    geom <- Geom$find(layer$geom$guide_geom())
    params <- c(layer$geom_params, layer$stat_params)
    list(geom = geom, data = data, params = params)
  }
  )

  ## remove null geom
  guide$geoms <- guide$geoms[!sapply(guide$geoms, is.null)]
  guide
}

guide_gengrob.legend <- function(guide, theme) {

  ## default setting
  label.position <- guide$label.position %||% "right"
  if (!label.position %in% c("top", "bottom", "left", "right")) stop("label position \"", label.position, "\" is invalid")
  
  nbreak <- nrow(guide$key)

  ## gap between keys etc
  hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  ## title
  ## TODO: hjust of title should depend on title.position
  title.hjust <- theme$legend.title.align %||% 0
  title.x <- theme$legend.title.align %||% 0
  title.vjust <- 0.5
  title.y <- 0.5
  grob.title <- {
    g <-
      if (is.null(guide$title)) zeroGrob()
      else if(!is.null(guide$title.theme)) guide$title.theme(label=guide$title, name=grobName(NULL, "guide.title"))
      else theme_render(theme, "legend.title", guide$title, hjust = title.hjust, vjust = title.vjust, x = title.x, y = title.y)
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
  ## TODO: adjust hjust based on the label position.
  ## 
  ## if labels are math expression, them it should be right-aligned. else left-aligned.
#  label.hjust <- guide$label.hjust %||% theme$legend.text.align %||% 0.5
#  label.x <- 0.5
#  label.vjust <- 0.5
#  label.y <- 0.5
#  if (is.na(theme$legend.text.align)) {
#    numeric_labels <- all(sapply(guide$key$.label, is.language)) || suppressWarnings(all(!is.na(sapply(guide$key$.label, "as.numeric"))))
#    hpos <- numeric_labels * 1    
#  } else {
#    hpos <- theme$legend.text.align
#  }
  grob.labels <-
    if (!guide$label) zeroGrob()
    else lapply(guide$key$.label, function(label){
      g <- 
        if(!is.null(guide$label.theme)) guide$label.theme(label=label, name=grobName(NULL, "guide.label"))
        else theme_render(theme, "legend.text", label, hjust = 0, x = 0, y = 0.5)
      if (!is.null(guide$label.angle)) g <- editGrob(g, rot = guide$label.angle)
      if (!is.null(guide$label.hjust)) g <- editGrob(g, hjust = guide$label.hjust)
      if (!is.null(guide$label.vjust)) g <- editGrob(g, vjust = guide$label.vjust)
      g
    })

  label_widths <- lapply(grob.labels, function(g)convertWidth(grobWidth(g), "mm"))
  label_heights <- lapply(grob.labels, function(g)convertHeight(grobHeight(g), "mm"))
  label_widths.c <- unlist(label_widths)
  label_heights.c <- unlist(label_heights)

  ## key size

  key_width <- convertWidth(guide$keywidth %||% theme$legend.key.width %||% theme$legend.key.size, "mm")
  key_height <- convertHeight(guide$keyheight %||% theme$legend.key.height %||% theme$legend.key.size, "mm")
  
  key_width.c <- c(key_width)
  key_height.c <- c(key_height)

  key_size_mat <- do.call("cbind", llply(guide$legend_data, "[[", "size"))
  key_sizes <- if (is.null(key_size_mat)) rep(0, nbreak) else apply(key_size_mat, 1, max)

  ## layout of key-label depends on the direction of the guide
  switch(guide$direction,
    "horizontal" = {
      key_widths.c <- pmax(key_width.c, key_sizes)
      key_height.c <-max(key_height.c, key_sizes)
      switch(label.position,
        "top" = {
          kl_widths <- interleave(pmax(label_widths.c, key_widths.c), rep(hgap, nbreak))
          kl_heights <- c(max(label_heights.c), vgap, max(key_height.c))
          vps <- list(key.row = rep(3, nbreak), key.col = seq(nbreak)*2-1,
                      label.row = rep(1, nbreak), label.col = seq(nbreak)*2-1)
        },
        "bottom" = {
          kl_widths <- interleave(pmax(label_widths.c, key_widths.c), rep(hgap, nbreak))
          kl_heights <- c(max(key_height.c), vgap, max(label_heights.c))
          vps <- list(key.row = rep(1, nbreak), key.col = seq(nbreak)*2-1,
                      label.row = rep(3, nbreak), label.col = seq(nbreak)*2-1)
        },
        "left" = {
          kl_widths <- interleave(label_widths.c, rep(hgap, nbreak), key_widths.c, rep(hgap, nbreak))
          kl_heights <- max(label_heights.c, key_height.c)
          vps <- list(key.row = rep(1, nbreak), key.col = seq(nbreak)*4-1,
                      label.row = rep(1, nbreak), label.col = seq(nbreak)*4-3)
        },
        "right" = {
          kl_widths <- interleave(key_widths.c, rep(hgap, nbreak), label_widths.c, rep(hgap, nbreak))
          kl_heights <- max(label_heights.c, key_height.c)
          vps <- list(key.row = rep(1, nbreak), key.col = seq(nbreak)*4-3,
                      label.row = rep(1, nbreak), label.col = seq(nbreak)*4-1)
          })
      kl_widths <- kl_widths[-length(kl_widths)] # remove the rightmost hgap
    },
    "vertical" = {
      key_width.c <- max(key_width.c, key_sizes)
      key_heights.c <-pmax(key_height.c, key_sizes)
      switch(label.position,
        "top" = {
          vps <- list(key.row = seq(nbreak)*4-1, key.col = rep(1, nbreak),
                      label.row = seq(nbreak)*4-3, label.col = rep(1, nbreak))
          kl_widths <- max(label_widths.c, key_width.c)
          kl_heights <- interleave(label_heights.c, rep(vgap, nbreak), key_heights.c, rep(vgap, nbreak))
        },
        "bottom" = {
          vps <- list(key.row = seq(nbreak)*4-3, key.col = rep(1, nbreak),
                      label.row = seq(nbreak)*4-1, label.col = rep(1, nbreak))
          kl_widths <- max(label_widths.c, key_width.c)
          kl_heights <- interleave(key_heights.c, rep(vgap, nbreak), label_heights.c, rep(vgap, nbreak))
        },
        "left" = {
          vps <- list(key.row = seq(nbreak)*2-1, key.col = rep(3, nbreak),
                      label.row = seq(nbreak)*2-1, label.col = rep(1, nbreak))
          kl_widths <- c(max(label_widths.c), hgap, max(key_width.c))
          kl_heights <- interleave(pmax(key_heights.c, label_heights.c), rep(vgap, nbreak))
        },
        "right" = {
          vps <- list(key.row = seq(nbreak)*2-1, key.col = rep(1, nbreak),
                      label.row = seq(nbreak)*2-1, label.col = rep(3, nbreak))
          kl_widths <- c(max(key_width.c), hgap, max(label_widths.c))
          kl_heights <- interleave(pmax(key_heights.c, label_heights.c), rep(vgap, nbreak))
        })
      kl_heights <- kl_heights[-length(kl_heights)] # remove the bottom vgap
    })

  ## layout the title over key-label
  switch(guide$title.position,
    "top" = {
      widths <- c(hgap, kl_widths, max(hgap, title_width.c-sum(kl_widths)))
      heights <- c(vgap, title_height.c, vgap, kl_heights, vgap)
      vps <- with(vps,
                  list(key.row = key.row+3, key.col = key.col+1,
                       label.row = label.row+3, label.col = label.col+1,
                       title.row = 2, title.col = 2:(length(widths)-1)))
    },
    "bottom" = {
      widths <- c(hgap, kl_widths, max(hgap, title_width.c-sum(kl_widths)))
      heights <- c(vgap, kl_heights, vgap, title_height.c, vgap)
      vps <- with(vps, 
                  list(key.row = key.row+1, key.col = key.col+1,
                       label.row = label.row+1, label.col = label.col+1,
                       title.row = length(heights)-1, title.col = 2:(length(widths)-1)))
    },
    "left" = {
      widths <- c(hgap, title_width.c, hgap, kl_widths, hgap)
      heights <- c(vgap, kl_heights, max(vgap, title_height.c-sum(kl_heights)))
      vps <- with(vps, 
                  list(key.row = key.row+1, key.col = key.col+3,
                       label.row = label.row+1, label.col = label.col+3,
                       title.row = 2:(length(heights)-1), title.col = 2))
    },
    "right" = {
      widths <- c(hgap, kl_widths, hgap, title_width.c, hgap)
      heights <- c(vgap, kl_heights, max(vgap, title_height.c-sum(kl_heights)))
      vps <- with(vps, 
                  list(key.row = key.row+1, key.col = key.col+1,
                       label.row = label.row+1, label.col = label.col+1,
                       title.row = 2:(length(heights)-1), title.col = length(widths)-1))
    })

  ## grob for key
  grob.key <- list()

  for (i in 1:nbreak) {

    ## layout position
    pos.row <- vps$key.row[i]
    pos.col <- vps$key.col[i]

    ## bg. of key
    grob.key[[length(grob.key)+1]] <- theme_render(theme, "legend.key", vp = viewport(layout.pos.row = pos.row, layout.pos.col = pos.col))

    ## overlay geoms
    for(geom in guide$geoms) {
      .key <- geom$geom$draw_legend(geom$data[i, ], geom$params)
      .key$vp <- viewport(layout.pos.row = pos.row, layout.pos.col = pos.col)
      grob.key[[length(grob.key)+1]] <- .key
    }

    ## layout position for label
    grob.labels[[i]]$vp <- viewport(layout.pos.row = vps$label.row[i], layout.pos.col = vps$label.col[i])
  }

  ## background
  grob.background <- theme_render(theme, "legend.background")

  ## layout position for title
  grob.title$vp <- viewport(layout.pos.row = vps$title.row, layout.pos.col = vps$title.col)

  ## layout of the legend guide
  ## TODO: just specification
  legend.layout <- grid.layout(length(heights), length(widths), 
                               widths = unit(widths, "mm"), heights = unit(heights, "mm"), 
                               just = "left")

  ## return grob tree 
  gTree(children = do.call("gList", c(list(grob.background, grob.title), grob.key, grob.labels)),
        vp = viewport(layout=legend.layout))
}
