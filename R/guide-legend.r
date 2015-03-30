#' Legend guide.
#'
#' Legend type guide shows key (i.e., geoms) mapped onto values.
#' Legend guides for various scales are integrated if possible.
#'
#' Guides can be specified in each scale or in \code{\link{guides}}.
#' \code{guide="legend"} in scale is syntactic sugar for
#' \code{guide=guide_legend()}. As for how to specify the guide for each
#' scales in more detail, see \code{\link{guides}}.
#'
#' @param title A character string or expression indicating a title of guide.
#'   If \code{NULL}, the title is not shown. By default
#'   (\code{\link{waiver}}), the name of the scale object or tha name
#'   specified in \code{\link{labs}} is used for the title.
#' @param title.position A character string indicating the position of a
#'   title. One of "top" (default for a vertical guide), "bottom", "left"
#'  (default for a horizontal guide), or "right."
#' @param title.theme A theme object for rendering the title text. Usually the
#'   object of \code{\link{element_text}} is expected. By default, the theme is
#'   specified by \code{legend.title} in \code{\link{theme}} or theme.
#' @param title.hjust A number specifying horizontal justification of the
#'   title text.
#' @param title.vjust A number specifying vertical justification of the title
#'   text.
#' @param label logical. If \code{TRUE} then the labels are drawn. If
#'   \code{FALSE} then the labels are invisible.
#' @param label.position A character string indicating the position of a
#'   label. One of "top", "bottom" (default for horizontal guide), "left", or
#'   "right" (default for vertical gudie).
#' @param label.theme A theme object for rendering the label text. Usually the
#'   object of \code{\link{element_text}} is expected. By default, the theme is
#'   specified by \code{legend.text} in \code{\link{theme}} or theme.
#' @param label.hjust A numeric specifying horizontal justification of the
#'   label text.
#' @param label.vjust A numeric specifying vertical justification of the label
#'   text.
#' @param keywidth A numeric or a unit object specifying the width of the
#'   legend key. Default value is \code{legend.key.width} or
#'   \code{legend.key.size} in \code{\link{theme}} or theme.
#' @param keyheight A numeric or a unit object specifying the height of the
#'   legend key. Default value is \code{legend.key.height} or
#'   \code{legend.key.size} in \code{\link{theme}} or theme.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating unit for \code{keywidth}
#'   and \code{keyheight}.
#' @param override.aes A list specifying aesthetic parameters of legend key.
#'   See details and examples.
#' @param nrow The desired number of rows of legends.
#' @param ncol The desired number of column of legends.
#' @param byrow logical. If \code{FALSE} (the default) the legend-matrix is
#'   filled by columns, otherwise the legend-matrix is filled by rows.
#' @param reverse logical. If \code{TRUE} the order of legends is reversed.
#' @param order positive integer less that 99 that specify the order of
#'   this guide in the multiple guides. If 0 (default), the order is determined
#'   by a secret algorithm.
#' @param ... ignored.
#' @return A guide object
#' @export
#' @family guides
#' @examples
#' \donttest{
#' library(reshape2) # for melt
#' df <- melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#'
#' p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#' p2 <- p1 + geom_point(aes(size = value))
#'
#' # Basic form
#' p1 + scale_fill_continuous(guide = "legend")
#' p1 + scale_fill_continuous(guide = guide_legend())
#'
#' # Guide title
#'
#' p1 + scale_fill_continuous(guide = guide_legend(title = "V")) # title text
#' p1 + scale_fill_continuous(name = "V") # same
#' p1 + scale_fill_continuous(guide = guide_legend(title = NULL)) # no title
#'
#' # Control styles
#'
#' # key size
#' p1 + guides(fill = guide_legend(keywidth = 3, keyheight = 1))
#'
#' # title position
#' p1 + guides(fill = guide_legend(title = "LEFT", title.position = "left"))
#'
#' # title text styles via element_text
#' p1 + guides(fill = guide_legend(
#'   title.theme = element_text(size=15, face="italic", colour = "red", angle = 45)))
#'
#' # label position
#' p1 + guides(fill = guide_legend(label.position = "bottom"))
#'
#' # label styles
#' p1 + scale_fill_continuous(breaks = c(5, 10, 15),
#'   labels = paste("long", c(5, 10, 15)),
#'   guide = guide_legend(direction = "horizontal", title.position = "top",
#'     label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
#'     label.theme = element_text(angle = 90)))
#'
#' # Set aesthetic of legend key
#'
#' # very low alpha value make it difficult to see legend key
#' p3 <- qplot(carat, price, data = diamonds, colour = color,
#'   alpha = I(1/100))
#' p3
#'
#' # override.aes overwrites the alpha
#' p3 + guides(colour = guide_legend(override.aes = list(alpha = 1)))
#'
#' # multiple row/col legends
#' p <- qplot(1:20, 1:20, colour = letters[1:20])
#' p + guides(col = guide_legend(nrow = 8))
#' p + guides(col = guide_legend(ncol = 8))
#' p + guides(col = guide_legend(nrow = 8, byrow = TRUE))
#' p + guides(col = guide_legend(ncol = 8, byrow = TRUE))
#'
#' # reversed order legend
#' p + guides(col = guide_legend(reverse = TRUE))
#' }
guide_legend <- function(

  # title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # key
  keywidth = NULL,
  keyheight = NULL,

  # general
  direction = NULL,
  default.unit = "line",
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  byrow = FALSE,
  reverse = FALSE,
  order = 0,

  ...) {

  if (!is.null(keywidth) && !is.unit(keywidth)) keywidth <- unit(keywidth, default.unit)
  if (!is.null(keyheight) && !is.unit(keyheight)) keyheight <- unit(keyheight, default.unit)

  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # size of key
    keywidth = keywidth,
    keyheight = keyheight,

    # general
    direction = direction,
    default.unit = default.unit,
    override.aes = override.aes,
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = c("any"),

    ..., name="legend"),
    class=c("guide", "legend"))
}

#' @export
guide_train.legend <- function(guide, scale) {
  breaks <- scale_breaks(scale)
  key <- data.frame(
    values = scale_map(scale, breaks),
    labels = I(scale_labels(scale)),
    stringsAsFactors = FALSE)

  # this is a quick fix for #118
  # some scales have NA as na.value (e.g., size)
  # some scales have non NA as na.value (e.g., "grey50" for colour)
  # drop rows if data (instead of the mapped value) is NA
  #
  # Also, drop out-of-range values for continuous scale
  # (should use scale$oob?)
  if (inherits(scale, "continuous")) {
    limits <- scale_limits(scale)
    noob <- !is.na(breaks) & limits[1] <= breaks & breaks <= limits[2]
    key <- key[noob, , drop = FALSE]
  } else {
    key <- key[!is.na(breaks), , drop = FALSE]
  }

  if (empty(key) || all(is.na(breaks))) return(NULL)
  names(key) <- c(scale$aesthetics[1], ".label")

  if (guide$reverse) key <- key[nrow(key):1, ]

  guide$key <- key
  guide$hash <- with(guide, digest(list(title, key$.label, direction, name)))
  guide
}

#' @export
guide_merge.legend <- function(guide, new_guide) {
  guide$key <- merge(guide$key, new_guide$key, sort=FALSE)
  guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
  if (any(duplicated(names(guide$override.aes)))) warning("Duplicated override.aes is ignored.")
  guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
  guide
}

#' @export
guide_geom.legend <- function(guide, layers, default_mapping) {

  # TODO: how to deal with same geoms of multiple layers.
  #
  # currently all geoms are overlayed irrespective to that they are duplicated or not.
  # but probably it is better to sensitive to that and generate only one geom like this:
  #
  # geoms <- unique(sapply(layers, function(layer) if (is.na(layer$legend) || layer$legend) layer$geom$guide_geom() else NULL))
  #
  # but in this case, some conflicts occurs, e.g.,
  #
  # d <- data.frame(x=1:5, y=1:5, v=factor(1:5))
  # ggplot(d, aes(x, y, colour=v, group=1)) + geom_point() + geom_line(colour="red", legend=T) + geom_rug(colour="blue", legend=T)
  #
  # geom_line generate path geom with red and geom_rug generate it with blue.
  # how to deal with them ?

  # arrange common data for vertical and horizontal guide
  guide$geoms <- llply(layers, function(layer) {
    all <- names(c(layer$mapping, default_mapping, layer$stat$default_aes()))
    geom <- c(layer$geom$required_aes, names(layer$geom$default_aes()))
    matched <- intersect(intersect(all, geom), names(guide$key))
    matched <- setdiff(matched, names(layer$geom_params))
    data <-
      if (length(matched) > 0) {
        # This layer contributes to the legend
        if (is.na(layer$show_guide) || layer$show_guide) {
          # Default is to include it
          layer$use_defaults(guide$key[matched])
        } else {
          NULL
        }
      } else {
        # This layer does not contribute to the legend
        if (is.na(layer$show_guide) || !layer$show_guide) {
          # Default is to exclude it
          NULL
        } else {
          layer$use_defaults(NULL)[rep(1, nrow(guide$key)), ]
        }
      }
    if (is.null(data)) return(NULL)

    # override.aes in guide_legend manually changes the geom
    for (aes in intersect(names(guide$override.aes), names(data))) data[[aes]] <- guide$override.aes[[aes]]

    geom <- Geom$find(layer$geom$guide_geom())
    params <- c(layer$geom_params, layer$stat_params)
    list(geom = geom, data = data, params = params)
  }
  )

  # remove null geom
  guide$geoms <- compact(guide$geoms)

  # Finally, remove this guide if no layer is drawn
  if (length(guide$geoms) == 0) guide <- NULL
  guide
}

#' @export
guide_gengrob.legend <- function(guide, theme) {

  # default setting
  label.position <- guide$label.position %||% "right"
  if (!label.position %in% c("top", "bottom", "left", "right"))
    stop("label position \"", label.position, "\" is invalid")

  nbreak <- nrow(guide$key)

  # gap between keys etc
  hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  # title
  title.theme <- guide$title.theme %||% calc_element("legend.title", theme)
  title.hjust <- title.x <- guide$title.hjust %||% theme$legend.title.align %||% 0
  title.vjust <- title.y <- guide$title.vjust %||% 0.5

  grob.title <- {
    if (is.null(guide$title))
      zeroGrob()
    else {
      g <- element_grob(title.theme, label=guide$title,
        hjust = title.hjust, vjust = title.vjust, x = title.x, y = title.y)
      ggname("guide.title", g)
    }
  }

  title_width <- convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  # Label
  # Rules of lable adjustment
  #
  # label.theme in param of guide_legend() > theme$legend.text.align > default
  # hjust/vjust in theme$legend.text and label.theme are ignored.
  #
  # Default:
  #   If label includes expression, the label is right-alignd (hjust = 0). Ohterwise, left-aligned (x = 1, hjust = 1).
  #   Vertical adjustment is always mid-alined (vjust = 0.5).
  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)
  grob.labels <- {
    if (!guide$label)
      zeroGrob()
    else {
      hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
        if (any(is.expression(guide$key$.label))) 1 else 0
      vjust <- y <- guide$label.vjust %||% 0.5

      lapply(guide$key$.label,
        function(label, ...) {
          g <- element_grob(element = label.theme, label = label,
            x = x, y = y, hjust = hjust, vjust = vjust)
          ggname("guide.label", g)
        }
      )
    }
  }

  label_widths <- lapply(grob.labels, function(g)convertWidth(grobWidth(g), "mm"))
  label_heights <- lapply(grob.labels, function(g)convertHeight(grobHeight(g), "mm"))
  label_widths.c <- unlist(label_widths)
  label_heights.c <- unlist(label_heights)

  # key size

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

  # layout of key-label depends on the direction of the guide
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

  # layout the title over key-label
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

  # grob for key
  grob.keys <- list()

  for (i in 1:nbreak) {

    # layout position
    pos.row <- vps$key.row[i]
    pos.col <- vps$key.col[i]

    # bg. of key
    grob.keys[[length(grob.keys)+1]] <- element_render(theme, "legend.key")

    # overlay geoms
    for(geom in guide$geoms)
      grob.keys[[length(grob.keys)+1]] <- geom$geom$draw_legend(geom$data[i, ], geom$params)
  }

  # background
  grob.background <- element_render(theme, "legend.background")

  ngeom <- length(guide$geoms) + 1
  kcols <- rep(vps$key.col, each =  ngeom)
  krows <- rep(vps$key.row, each =  ngeom)

  # padding
  padding <- unit(1.5, "mm")
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)

  # Create the gtable for the legend
  gt <- gtable(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.title, name = "title", clip = "off",
    t = 1 + min(vps.title.row), r = 1 + max(vps.title.col),
    b = 1 + max(vps.title.row), l = 1 + min(vps.title.col))
  gt <- gtable_add_grob(gt, grob.keys,
    name = paste("key", krows, kcols, c("bg", seq(ngeom-1)), sep = "-"), clip = "off",
    t = 1 + krows, r = 1 + kcols,
    b = 1 + krows, l = 1 + kcols)
  gt <- gtable_add_grob(gt, grob.labels,
    name = paste("label", vps$label.row, vps$label.col, sep = "-"), clip = "off",
    t = 1 + vps$label.row, r = 1 + vps$label.col,
    b = 1 + vps$label.row, l = 1 + vps$label.col)

  gt
}

globalVariables(c("R", "key.row", "key.col", "label.row", "label.col"))
