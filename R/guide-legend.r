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

    ## parameter
    available_aes = c("any"),

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
  guide$key <- merge(guide$key, new_guide$key, sort=FALSE)
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
  title.hjust <- title.x <- guide$title.hjust %||% theme$legend.title.align %||% 0
  title.vjust <- title.y <- guide$title.vjust %||% 0.5
  grob.title <- {
    g <-
      if (is.null(guide$title))
        zeroGrob()
      else if(!is.null(guide$title.theme))
        guide$title.theme(label=guide$title, name=grobName(NULL, "guide.title"),
          hjust = title.hjust, vjust = title.vjust, x = title.x, y = title.y)
      else
        theme_render(theme, "legend.title", guide$title,
          hjust = title.hjust, vjust = title.vjust, x = title.x, y = title.y)
    if (!is.null(guide$title.angle)) g <- editGrob(g, rot = guide$title.angle)
    g
  }

  title_width <- convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  ## Label
  ## Rules of lable adjustment
  ##
  ## label.theme in param of guide_legend() > theme$legend.text.align > default
  ## hjust/vjust in theme$legend.text and label.theme are ignored.
  ## 
  ## Default:
  ##   If label includes expression, the label is right-alignd (hjust = 0). Ohterwise, left-aligned (x = 1, hjust = 1).
  ##   Vertical adjustment is always mid-alined (vjust = 0.5).
  grob.labels <-
    if (!guide$label) zeroGrob()
    else {
      hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||% if (any(is.expression(guide$key$.label))) 1 else 0
      vjust <- y <- guide$label.vjust %||% 0.5
      lapply(guide$key$.label, function(label){
        g <-
          if(!is.null(guide$label.theme))
            guide$label.theme(label=label, name=grobName(NULL, "guide.label"), x = x, y = y, hjust = hjust, vjust = vjust)
          else
            theme_render(theme, "legend.text", label, x = x, y = y, hjust = hjust, vjust = vjust)
        if (!is.null(guide$label.angle)) g <- editGrob(g, rot = guide$label.angle)
        g
      })
    }

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
          kl_widths <- pmax(label_widths.c, key_widths.c)
          kl_heights <- c(max(label_heights.c), vgap, max(key_height.c))
          vps <- list(key.row = rep(3, nbreak), key.col = seq(nbreak),
                      label.row = rep(1, nbreak), label.col = seq(nbreak))
        },
        "bottom" = {
          kl_widths <- pmax(label_widths.c, key_widths.c)
          kl_heights <- c(max(key_height.c), vgap, max(label_heights.c))
          vps <- list(key.row = rep(1, nbreak), key.col = seq(nbreak),
                      label.row = rep(3, nbreak), label.col = seq(nbreak))
        },
        "left" = {
          kl_widths <- head(interleave(label_widths.c, rep(hgap/2, nbreak), key_widths.c, rep(hgap, nbreak)), -1)
          kl_heights <- max(label_heights.c, key_height.c)
          vps <- list(key.row = rep(1, nbreak), key.col = seq(nbreak)*4-1,
                      label.row = rep(1, nbreak), label.col = seq(nbreak)*4-3)
        },
        "right" = {
          kl_widths <- head(interleave(key_widths.c, rep(hgap/2, nbreak), label_widths.c, rep(hgap, nbreak)), -1)
          kl_heights <- max(label_heights.c, key_height.c)
          vps <- list(key.row = rep(1, nbreak), key.col = seq(nbreak)*4-3,
                      label.row = rep(1, nbreak), label.col = seq(nbreak)*4-1)
          })      
    },
    "vertical" = {
      key_width.c <- max(key_width.c, key_sizes)
      key_heights.c <-pmax(key_height.c, key_sizes)
      switch(label.position,
        "top" = {
          kl_widths <- max(label_widths.c, key_width.c)
          kl_heights <- head(interleave(label_heights.c, rep(vgap/2, nbreak), key_heights.c, rep(vgap, nbreak)), -1)
          vps <- list(key.row = seq(nbreak)*4-1, key.col = rep(1, nbreak),
                      label.row = seq(nbreak)*4-3, label.col = rep(1, nbreak))
        },
        "bottom" = {
          kl_widths <- max(label_widths.c, key_width.c)
          kl_heights <- head(interleave(key_heights.c, rep(vgap/2, nbreak), label_heights.c, rep(vgap, nbreak)), -1)
          vps <- list(key.row = seq(nbreak)*4-3, key.col = rep(1, nbreak),
                      label.row = seq(nbreak)*4-1, label.col = rep(1, nbreak))
        },
        "left" = {
          kl_widths <- c(max(label_widths.c), hgap, max(key_width.c))
          kl_heights <- pmax(key_heights.c, label_heights.c)
          vps <- list(key.row = seq(nbreak), key.col = rep(3, nbreak),
                      label.row = seq(nbreak), label.col = rep(1, nbreak))
        },
        "right" = {
          kl_widths <- c(max(key_width.c), hgap, max(label_widths.c))
          kl_heights <- pmax(key_heights.c, label_heights.c)
          vps <- list(key.row = seq(nbreak), key.col = rep(1, nbreak),
                      label.row = seq(nbreak), label.col = rep(3, nbreak))
        })
    })

  ## layout the title over key-label
  switch(guide$title.position,
    "top" = {
      widths <- c(kl_widths, max(0, title_width.c-sum(kl_widths)))
      heights <- c(title_height.c, vgap, kl_heights)
      vps <- with(vps,
                  list(key.row = key.row+2, key.col = key.col,
                       label.row = label.row+2, label.col = label.col,
                       title.row = 1, title.col = 1:length(widths)))
    },
    "bottom" = {
      widths <- c(kl_widths, max(0, title_width.c-sum(kl_widths)))
      heights <- c(kl_heights, vgap, title_height.c)
      vps <- with(vps, 
                  list(key.row = key.row, key.col = key.col,
                       label.row = label.row, label.col = label.col,
                       title.row = length(heights), title.col = 1:length(widths)))
    },
    "left" = {
      widths <- c(title_width.c, hgap, kl_widths)
      heights <- c(kl_heights, max(0, title_height.c-sum(kl_heights)))
      vps <- with(vps, 
                  list(key.row = key.row, key.col = key.col+2,
                       label.row = label.row, label.col = label.col+2,
                       title.row = 1:length(heights), title.col = 1))
    },
    "right" = {
      widths <- c(kl_widths, hgap, title_width.c)
      heights <- c(kl_heights, max(0, title_height.c-sum(kl_heights)))
      vps <- with(vps, 
                  list(key.row = key.row, key.col = key.col,
                       label.row = label.row, label.col = label.col,
                       title.row = 1:length(heights), title.col = length(widths)))
    })

  ## grob for key
  grob.keys <- list()
  
  for (i in 1:nbreak) {

    ## layout position
    pos.row <- vps$key.row[i]
    pos.col <- vps$key.col[i]

    ## bg. of key
    grob.keys[[length(grob.keys)+1]] <- theme_render(theme, "legend.key")

    ## overlay geoms
    for(geom in guide$geoms)
      grob.keys[[length(grob.keys)+1]] <- geom$geom$draw_legend(geom$data[i, ], geom$params)
  }

  ## background
  grob.background <- theme_render(theme, "legend.background")

  ngeom <- length(guide$geoms) + 1
  kcols <- rep(vps$key.col, each =  ngeom)
  krows <- rep(vps$key.row, each =  ngeom)
  lay <- data.frame(l = c(1,               min(vps$title.col), kcols, vps$label.col),
                    t = c(1,               min(vps$title.row), krows, vps$label.row),
                    r = c(length(widths),  max(vps$title.col), kcols, vps$label.col),
                    b = c(length(heights), max(vps$title.row), krows, vps$label.row),
                    name = c("background", "title",
                      paste("key", krows, kcols, c("bg", seq(ngeom-1)), sep = "-"),
                      paste("label", vps$label.row, vps$label.col, sep = "-")),
                    clip = FALSE)

  gtable(c(list(grob.background, grob.title), grob.keys, grob.labels), lay, unit(widths, "mm"), unit(heights, "mm"))
}
