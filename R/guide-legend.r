##' Legend type guide
##'
##' Legend type guide shows key (i.e., geoms) mapped onto values.
##' Legend guides for various scales are integrated if possible.
##' 
##' Guides can be specified in each scale or in \code{\link{guides}}.
##' \code{guide="legend"} in scale is syntax suger for \code{guide=guide_legend()}.
##' As for how to specify the guide for each scales in more detail, see \code{\link{guides}}.
##' 
##' @name guide_legend
##' @title Legend guide
##' @param title A character string or expression indicating a title of guide. If \code{NULL}, the title is not shown. By default (\code{\link{waiver()}}), the name of the scale object or tha name specified in \code{\link{labs}} is used for the title.
##' @param title.position A character string indicating the position of a title. One of "top" (default for a vertical guide), "bottom", "left" (default for a horizontal guide), or "right."
##' @param title.theme A theme object for rendering the title text. Usually the object of \code{\link{theme_text}} is expected. By default, the theme is specified by \code{legend.title} in \code{\link{opts}} or theme.
##' @param title.hjust A numeric specifying horizontal justification of the title text.
##' @param title.vjust A numeric specifying vertical justification of the title text.
##' @param label logical. If \code{TRUE} then the labels are drawn. If \code{FALSE} then the labels are invisible.
##' @param label.position A character string indicating the position of a label. One of "top", "bottom", "left", or "right" (default).
##' @param label.theme A theme object for rendering the label text. Usually the object of \code{\link{theme_text}} is expected. By default, the theme is specified by \code{legend.text} in \code{\link{opts}} or theme.
##' @param label.hjust A numeric specifying horizontal justification of the label text.
##' @param label.vjust A numeric specifying vertical justification of the label text.
##' @param keywidth A numeric or a unit object specifying the width of the legend key. Default value is \code{legend.key.width} or \code{legend.key.size} in \code{\link{opts}} or theme.
##' @param keyheight A numeric or a unit object specifying the height of the legend key. Default value is \code{legend.key.height} or \code{legend.key.size} in \code{\link{opts}} or theme.
##' @param direction A character string indicating the direction of the guide. One of "horizontal" or "vertical."
##' @param default.unit A character string indicating unit for \code{keywidth} and \code{keyheight}.
##' @param override.aes A list specifying aesthetic parameters of legend key. See details and examples.
##' @param nrow The desired number of rows of legends.
##' @param ncol The desired number of column of legends.
##' @param byrow logical. If \code{FALSE} (the default) the legend-matrix is filled by columns, otherwise the legend-matrix is filled by rows.
##' @param reverse logical. If \code{TRUE} the order of legends is reversed.
##' @param ... ignored.
##' @return Guide object
##' @seealso \code{\link{guides}}, \code{\link{guide_colorbar}}
##' @export
##' @examples
##' p1 <- function()ggplot(melt(outer(1:4, 1:4), varnames = c("X1", "X2")), aes(x = X1, y = X2)) + geom_tile(aes(fill = value))
##' p2 <- function()ggplot(melt(outer(1:4, 1:4), varnames = c("X1", "X2")), aes(x = X1, y = X2)) + geom_tile(aes(fill = value)) + geom_point(aes(size = value))
##' 
##' # basic form
##' ##'
##' # short version
##' p1() + scale_fill_continuous(guide = "legend")
##' ##'
##' # long version
##' p1() + scale_fill_continuous(guide = guide_legend())
##' ##'
##' # separately set the direction of each guide
##' 
##' p2() + scale_fill_continuous(guide = guide_legend(direction = "horizontal")) +
##'   scale_size(guide = guide_legend(direction = "vertical"))
##' 
##' # guide title
##' 
##' p1() + scale_fill_continuous(guide = guide_legend(title = "V")) # title text
##' p1() + scale_fill_continuous(name = "V") # same
##' p1() + scale_fill_continuous(guide = guide_legend(title = NULL)) # no title
##' 
##' # control styles
##' 
##' # key size
##' p1() + scale_fill_continuous(guide = guide_legend(keywidth = 3, keyheight = 1))
##' 
##' # title position
##' p1() + scale_fill_continuous(guide = guide_legend(title = "LEFT", title.position = "left"))
##' 
##' # title text styles via theme_text
##' p1() + scale_fill_continuous(guide = guide_legend(title.theme = theme_text(size=15, face="italic", col="red", angle=45)))
##' 
##' # label position
##' p1() + scale_fill_continuous(guide = guide_legend(label.position = "bottom"))
##' 
##' # label styles
##' p1() + scale_fill_continuous(breaks=c(5, 10, 15), labels=paste("long", c(5, 10, 15)), 
##'                             guide = guide_legend(direction="horizontal", title.position="top",
##'                               label.position="bottom", label.hjust = 0.5, label.vjust = 0.5, label.theme = theme_text(angle = 90)))
##' 
##' # set aesthetic of legend key
##' 
##' # very low alpha value make it difficult to see legend key
##' p3 <- function()ggplot(melt(outer(1:4, 1:4), varnames = c("X1", "X2")), aes(x = X1, y = X2))  + geom_tile(aes(fill = value), alpha = 0.1)
##' p3()
##' 
##' # override.aes overwrites the alpha
##' p3() + scale_fill_continuous(guide=guide_legend(override.aes = list(alpha=1)))
##' 
##' # combine colorbar and legend guide
##' p2() + scale_fill_continuous(guide = "colorbar") + scale_size(guide = "legend")
##' 
##' # same, but short version
##' p2() + guides(fill = "colorbar", size = "legend")
##'
##' # multiple row/col legends
##' p <- qplot(1:20, 1:20, col=letters[1:20])
##' p + guides(col=guide_legend(nrow=8))
##' p + guides(col=guide_legend(ncol=8))
##' p + guides(col=guide_legend(nrow=8, byrow=T))
##' p + guides(col=guide_legend(ncol=8, byrow=T))
##' 
##' # reversed order legend
##' p + guides(col=guide_legend(reverse = TRUE))

guide_legend <- function(
                         
  ##　title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  ## label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
                         
  ## key
  keywidth = NULL,
  keyheight = NULL,

  ## general
  direction = NULL,
  default.unit = "line",
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  byrow = FALSE,
  reverse = FALSE,
                         
  ...) {
  
  if (!is.null(keywidth) && !is.unit(keywidth)) keywidth <- unit(keywidth, default.unit)
  if (!is.null(keyheight) && !is.unit(keyheight)) keyheight <- unit(keyheight, default.unit)
  
  structure(list(
    ##　title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,
                 
    ## label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    ## size of key
    keywidth = keywidth,
    keyheight = keyheight,

    ## general
    direction = direction,
    default.unit = default.unit,
    override.aes = override.aes,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    reverse = reverse,
                 
    ## parameter
    available_aes = c("any"),

    ..., name="legend"),
    class=c("guide", "legend"))
}

guide_train.legend <- function(guide, scale) {
  breaks <- scale_breaks(scale)
  key <- data.frame(
    values = scale_map(scale, breaks),
    labels = scale_labels(scale),
    stringsAsFactors = FALSE)
  key <- key[!is.na(breaks), , drop = FALSE]
  names(key) <- c(scale$aesthetics[1], ".label")
  if (guide$reverse) key <- key[nrow(guide$key):1, ]
  
  guide$key <- key
  guide$hash <- with(guide, digest(list(title, key$.label, direction, name)))
  guide
}

guide_merge.legend <- function(guide, new_guide) {
  guide$key <- merge(guide$key, new_guide$key, sort=FALSE)
  guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
  if (any(duplicated(names(guide$override.aes)))) warning("Duplicated override.aes is ignored.")
  guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
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
    
    ## override.aes in guide_legend manually changes the geom
    for (aes in intersect(names(guide$override.aes), names(data))) data[[aes]] <- guide$override.aes[[aes]]

    geom <- Geom$find(layer$geom$guide_geom())
    params <- c(layer$geom_params, layer$stat_params)
    list(geom = geom, data = data, params = params)
  }
  )

  ## remove null geom
  guide$geoms <- guide$geoms[!sapply(guide$geoms, is.null)]

  ## Finally, remove this guide if no layer is drawn
  if (length(guide$geoms) == 0) guide <- NULL
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
  title.theme <- guide$title.theme %||% theme$legend.title
  title.hjust <- title.x <- guide$title.hjust %||% theme$legend.title.align %||% 0
  title.vjust <- title.y <- guide$title.vjust %||% 0.5
  
  grob.title <- {
    if (is.null(guide$title))
      zeroGrob()
    else 
      title.theme(label=guide$title, name=grobName(NULL, "guide.title"),
                  hjust = title.hjust, vjust = title.vjust, x = title.x, y = title.y)
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
  label.theme <- guide$label.theme %||% theme$legend.text
  grob.labels <- {
    if (!guide$label)
      zeroGrob()
    else {
      hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||% if (any(is.expression(guide$key$.label))) 1 else 0
      vjust <- y <- guide$label.vjust %||% 0.5
      lapply(guide$key$.label, function(label) label.theme(label=label, name=grobName(NULL, "guide.label"),
                                                           x = x, y = y, hjust = hjust, vjust = vjust))
    }
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

  key_size_mat <- do.call("cbind", llply(guide$geoms, function(g) g$data$size))
  key_sizes <- if (is.null(key_size_mat)) rep(0, nbreak) else apply(key_size_mat, 1, max)

  if (!is.null(guide$nrow) && !is.null(guide$ncol) && guide$nrow * guide$ncol < nbreak) 
    stop("nrow x ncol need to be larger than the number of breaks")
  legend.nrow <- guide$nrow %||%
    if (!is.null(guide$ncol)) ceiling(nbreak/guide$ncol)
    else switch(guide$direction, horizontal = 1, vertical = nbreak)
  legend.ncol <- guide$ncol %||%
    if (!is.null(guide$nrow)) ceiling(nbreak/guide$nrow)
    else switch(guide$direction, horizontal = nbreak, vertical = 1)
  key_sizes <- matrix(c(key_sizes, rep(0, legend.nrow * legend.ncol - nbreak)),
                      legend.nrow, legend.ncol, byrow = guide$byrow)

  key_widths.c <- pmax(key_width.c, apply(key_sizes, 2, max))
  key_heights.c <-pmax(key_height.c, apply(key_sizes, 1, max))
  
  label_widths.c <- apply(matrix(c(label_widths.c, rep(0, legend.nrow * legend.ncol - nbreak)),
                                 legend.nrow, legend.ncol, byrow = guide$byrow),
                          2, max)
  label_heights.c <- apply(matrix(c(label_heights.c, rep(0, legend.nrow * legend.ncol - nbreak)),
                                  legend.nrow, legend.ncol, byrow = guide$byrow),
                           1, max)

  if (guide$byrow) vps <- data.frame(ceiling(seq(nbreak)/legend.ncol), (seq(nbreak)-1)%%legend.ncol+1)
  else vps <- data.frame(arrayInd(seq(nbreak), dim(key_sizes)))
  names(vps) <- c("R", "C")

  ## layout of key-label depends on the direction of the guide
  if (guide$byrow == TRUE) {
    switch(label.position,
      "top" = {
        kl_widths <- pmax(label_widths.c, key_widths.c)
        kl_heights <- head(interleave(label_heights.c, vgap/2, key_heights.c, vgap/2), -1)
        vps <- transform(vps, key.row = R*4-1, key.col = C, label.row = R*4-3, label.col = C)
      },
      "bottom" = {
        kl_widths <- pmax(label_widths.c, key_widths.c)
        kl_heights <- head(interleave(key_heights.c, vgap/2, label_heights.c, vgap/2), -1)
        vps <- transform(vps, key.row = R*4-3, key.col = C, label.row = R*4-1, label.col = C)
      },
      "left" = {
        kl_widths <- head(interleave(label_widths.c, hgap/2, key_widths.c, hgap/2), -1)
        kl_heights <- head(interleave(pmax(label_heights.c, key_heights.c), vgap/2), -1)
        vps <- transform(vps, key.row = R*2-1, key.col = C*4-1, label.row = R*2-1, label.col = C*4-3)
      },
      "right" = {
        kl_widths <- head(interleave(key_widths.c, hgap/2, label_widths.c, hgap/2), -1)
        kl_heights <- head(interleave(pmax(label_heights.c, key_heights.c), vgap/2), -1)
        vps <- transform(vps, key.row = R*2-1, key.col = C*4-3, label.row = R*2-1, label.col = C*4-1)
        })      
  } else {
    switch(label.position,
      "top" = {
        kl_widths <- head(interleave(pmax(label_widths.c, key_widths.c), hgap/2), -1)
        kl_heights <- head(interleave(label_heights.c, vgap/2, key_heights.c, vgap/2), -1)
        vps <- transform(vps, key.row = R*4-1, key.col = C*2-1, label.row = R*4-3, label.col = C*2-1)
      },
      "bottom" = {
        kl_widths <- head(interleave(pmax(label_widths.c, key_widths.c), hgap/2), -1)
        kl_heights <- head(interleave(key_heights.c, vgap/2, label_heights.c, vgap/2), -1)
        vps <- transform(vps, key.row = R*4-3, key.col = C*2-1, label.row = R*4-1, label.col = C*2-1)
      },
      "left" = {
        kl_widths <- head(interleave(label_widths.c, hgap/2, key_widths.c, hgap/2), -1)
        kl_heights <- pmax(key_heights.c, label_heights.c)
        vps <- transform(vps, key.row = R, key.col = C*4-1, label.row = R, label.col = C*4-3)
      },
      "right" = {
        kl_widths <- head(interleave(key_widths.c, hgap/2, label_widths.c, hgap/2), -1)
        kl_heights <- pmax(key_heights.c, label_heights.c)
        vps <- transform(vps, key.row = R, key.col = C*4-3, label.row = R, label.col = C*4-1)
      })
  }

  ## layout the title over key-label
  switch(guide$title.position,
    "top" = {
      widths <- c(kl_widths, max(0, title_width.c-sum(kl_widths)))
      heights <- c(title_height.c, vgap, kl_heights)
      vps <- transform(vps, key.row = key.row+2, key.col = key.col, label.row = label.row+2, label.col = label.col)
      vps.title.row = 1; vps.title.col = 1:length(widths)
    },
    "bottom" = {
      widths <- c(kl_widths, max(0, title_width.c-sum(kl_widths)))
      heights <- c(kl_heights, vgap, title_height.c)
      vps <- transform(vps, key.row = key.row, key.col = key.col, label.row = label.row, label.col = label.col)
      vps.title.row = length(heights); vps.title.col = 1:length(widths)
    },
    "left" = {
      widths <- c(title_width.c, hgap, kl_widths)
      heights <- c(kl_heights, max(0, title_height.c-sum(kl_heights)))
      vps <- transform(vps, key.row = key.row, key.col = key.col+2, label.row = label.row, label.col = label.col+2)
      vps.title.row = 1:length(heights); vps.title.col = 1
    },
    "right" = {
      widths <- c(kl_widths, hgap, title_width.c)
      heights <- c(kl_heights, max(0, title_height.c-sum(kl_heights)))
      vps <- transform(vps, key.row = key.row, key.col = key.col, label.row = label.row, label.col = label.col)
      vps.title.row = 1:length(heights); vps.title.col = length(widths)
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
  lay <- data.frame(l = c(1,               min(vps.title.col), kcols, vps$label.col),
                    t = c(1,               min(vps.title.row), krows, vps$label.row),
                    r = c(length(widths),  max(vps.title.col), kcols, vps$label.col),
                    b = c(length(heights), max(vps.title.row), krows, vps$label.row),
                    name = c("background", "title",
                      paste("key", krows, kcols, c("bg", seq(ngeom-1)), sep = "-"),
                      paste("label", vps$label.row, vps$label.col, sep = "-")),
                    clip = FALSE)

  gtable(c(list(grob.background, grob.title), grob.keys, grob.labels), lay, unit(widths, "mm"), unit(heights, "mm"))
}
