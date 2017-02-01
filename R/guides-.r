#' Set guides for each scale
#'
#' Guides for each scale can be set scale-by-scale with the \code{guide}
#' argument, or en masse with \code{guides()}.
#'
#' @param ... List of scale name-guide pairs.  The guide can either
#'   be a string (i.e. "colorbar" or "legend"), or a call to a guide function
#'   (i.e. \code{\link{guide_colourbar}} or \code{\link{guide_legend}})
#'   specifying additional arguments.
#' @return A list containing the mapping between scale and guide.
#' @export
#' @family guides
#' @examples
#' \donttest{
#' # ggplot object
#'
#' dat <- data.frame(x = 1:5, y = 1:5, p = 1:5, q = factor(1:5),
#'  r = factor(1:5))
#' p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r)) + geom_point()
#'
#' # without guide specification
#' p
#'
#' # Show colorbar guide for colour.
#' # All these examples below have a same effect.
#'
#' p + guides(colour = "colorbar", size = "legend", shape = "legend")
#' p + guides(colour = guide_colorbar(), size = guide_legend(),
#'   shape = guide_legend())
#' p +
#'  scale_colour_continuous(guide = "colorbar") +
#'  scale_size_discrete(guide = "legend") +
#'  scale_shape(guide = "legend")
#'
#'  # Remove some guides
#'  p + guides(colour = "none")
#'  p + guides(colour = "colorbar",size = "none")
#'
#' # Guides are integrated where possible
#'
#' p + guides(colour = guide_legend("title"), size = guide_legend("title"),
#'   shape = guide_legend("title"))
#' # same as
#' g <- guide_legend("title")
#' p + guides(colour = g, size = g, shape = g)
#'
#' p + theme(legend.position = "bottom")
#'
#' # position of guides
#'
#' # Set order for multiple guides
#' ggplot(mpg, aes(displ, cty)) +
#'   geom_point(aes(size = hwy, colour = cyl, shape = drv)) +
#'   guides(
#'    colour = guide_colourbar(order = 1),
#'    shape = guide_legend(order = 2),
#'    size = guide_legend(order = 3)
#'  )
#' }
guides <- function(...) {
  args <- list(...)
  if (is.list(args[[1]]) && !inherits(args[[1]], "guide")) args <- args[[1]]
  args <- rename_aes(args)
  structure(args, class = "guides")
}

update_guides <- function(p, guides) {
  p <- plot_clone(p)
  p$guides <- defaults(guides, p$guides)
  p
}


# building guides - called in ggplotGrob (plot-render.r)
#
# the procedure is as follows:
#
# 1. guides_train()
#      train each scale and generate guide definition for all guides
#      here, one gdef for one scale
#
# 2. guides_merge()
#      merge gdefs if they are overlayed
#      number of gdefs may be less than number of scales
#
# 3. guides_geom()
#      process layer information and generate geom info.
#
# 4. guides_gengrob()
#      generate ggrob from each gdef
#      one ggrob for one gdef
#
# 5. guides_build()
#      arrange all ggrobs

build_guides <- function(scales, layers, default_mapping, position, theme, guides, labels) {
  theme$legend.key.width <- theme$legend.key.width %||% theme$legend.key.size
  theme$legend.key.height <- theme$legend.key.height %||% theme$legend.key.size

  # Layout of legends depends on their overall location
  position <- legend_position(position)
  if (position == "inside") {
    theme$legend.box <- theme$legend.box %||% "vertical"
    theme$legend.direction <- theme$legend.direction %||% "vertical"
    theme$legend.box.just <- theme$legend.box.just %||% c("center", "center")
  } else if (position == "vertical") {
    theme$legend.box <- theme$legend.box %||% "vertical"
    theme$legend.direction <- theme$legend.direction %||% "vertical"
    theme$legend.box.just <- theme$legend.box.just %||% c("left", "top")
  } else if (position == "horizontal") {
    theme$legend.box <- theme$legend.box %||% "horizontal"
    theme$legend.direction <- theme$legend.direction %||% "horizontal"
    theme$legend.box.just <- theme$legend.box.just %||% c("center", "top")
  }

  # scales -> data for guides
  gdefs <- guides_train(scales = scales, theme = theme, guides = guides, labels = labels)
  if (length(gdefs) == 0) return(zeroGrob())

  # merge overlay guides
  gdefs <- guides_merge(gdefs)

  # process layer information
  gdefs <- guides_geom(gdefs, layers, default_mapping)
  if (length(gdefs) == 0) return(zeroGrob())

  # generate grob of each guides
  ggrobs <- guides_gengrob(gdefs, theme)

  # build up guides
  grobs <- guides_build(ggrobs, theme)

  grobs
}

# Simplify legend position to one of horizontal/vertical/inside
legend_position <- function(position) {
  if (length(position) == 1) {
    if (position %in% c("top", "bottom")) {
      "horizontal"
    } else {
      "vertical"
    }
  } else {
    "inside"
  }
}

# validate guide object
validate_guide <- function(guide) {
  # if guide is specified by character, then find the corresponding guide
  if (is.character(guide))
    match.fun(paste("guide_", guide, sep = ""))()
  else if (inherits(guide, "guide"))
    guide
  else
    stop("Unknown guide: ", guide)
}

# train each scale in scales and generate the definition of guide
guides_train <- function(scales, theme, guides, labels) {

  gdefs <- list()
  for (scale in scales$scales) {

    # guides(XXX) is stored in guides[[XXX]],
    # which is prior to scale_ZZZ(guide=XXX)
    # guide is determined in order of:
    #   + guides(XXX) > + scale_ZZZ(guide=XXX) > default(i.e., legend)
    output <- scale$aesthetics[1]
    guide <- guides[[output]] %||% scale$guide

    # this should be changed to testing guide == "none"
    # scale$legend is backward compatibility
    # if guides(XXX=FALSE), then scale_ZZZ(guides=XXX) is discarded.
    if (guide == "none" || (is.logical(guide) && !guide)) next

    # check the validity of guide.
    # if guide is character, then find the guide object
    guide <- validate_guide(guide)

    # check the consistency of the guide and scale.
    if (guide$available_aes != "any" && !scale$aesthetics %in% guide$available_aes)
      stop("Guide '", guide$name, "' cannot be used for '", scale$aesthetics, "'.")

    guide$title <- scale$make_title(guide$title %|W|% scale$name %|W|% labels[[output]])

    # direction of this grob
    guide$direction <- guide$direction %||% theme$legend.direction

    # each guide object trains scale within the object,
    # so Guides (i.e., the container of guides) need not to know about them
    guide <- guide_train(guide, scale)

    if (!is.null(guide)) gdefs[[length(gdefs) + 1]] <- guide
  }
  gdefs
}

# merge overlapped guides
guides_merge <- function(gdefs) {
  # split gdefs based on hash, and apply Reduce (guide_merge) to each gdef group.
  gdefs <- lapply(gdefs, function(g) {
    if (g$order == 0) {
      order <- "99"
    } else {
      order <- sprintf("%02d", g$order)
    }
    g$hash <- paste(order, g$hash, sep = "_")
    g
  })
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(guide_merge, gs))
}

# process layer information
guides_geom <- function(gdefs, layers, default_mapping) {
  compact(lapply(gdefs, guide_geom, layers, default_mapping))
}

# generate grob from each gdef (needs to write this function?)
guides_gengrob <- function(gdefs, theme) {
  # common drawing process for all guides
  gdefs <- lapply(gdefs,
    function(g) {
      g$title.position <- g$title.position %||% switch(g$direction, vertical = "top", horizontal = "left")
      if (!g$title.position %in% c("top", "bottom", "left", "right"))
        stop("title position \"", g$title.position, "\" is invalid")
      g
    })

  lapply(gdefs, guide_gengrob, theme)
}

# build up all guide boxes into one guide-boxes.
guides_build <- function(ggrobs, theme) {
  theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
  theme$legend.spacing.y <- theme$legend.spacing.y  %||% theme$legend.spacing
  theme$legend.spacing.x <- theme$legend.spacing.x  %||% theme$legend.spacing

  widths <- do.call("unit.c", lapply(ggrobs, function(g)sum(g$widths)))
  heights <- do.call("unit.c", lapply(ggrobs, function(g)sum(g$heights)))

  # Set the justification of each legend within the legend box
  # First value is xjust, second value is yjust
  just <- valid.just(theme$legend.box.just)
  xjust <- just[1]
  yjust <- just[2]

  # setting that is different for vertical and horizontal guide-boxes.
  if (theme$legend.box == "horizontal") {
    # Set justification for each legend
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]] <- editGrob(ggrobs[[i]],
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
          height = heightDetails(ggrobs[[i]])))
    }

    guides <- gtable_row(name = "guides",
      grobs = ggrobs,
      widths = widths, height = max(heights))

    # add space between the guide-boxes
    guides <- gtable_add_col_space(guides, theme$legend.spacing.x)

  } else if (theme$legend.box == "vertical") {
    # Set justification for each legend
    for (i in seq_along(ggrobs)) {
      ggrobs[[i]] <- editGrob(ggrobs[[i]],
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
          width = widthDetails(ggrobs[[i]])))
    }

    guides <- gtable_col(name = "guides",
      grobs = ggrobs,
      width = max(widths), heights = heights)

    # add space between the guide-boxes
    guides <- gtable_add_row_space(guides, theme$legend.spacing.y)
  }

  # Add margins around the guide-boxes.
  theme$legend.box.margin <- theme$legend.box.margin %||% margin()
  guides <- gtable_add_cols(guides, theme$legend.box.margin[4], pos = 0)
  guides <- gtable_add_cols(guides, theme$legend.box.margin[2], pos = ncol(guides))
  guides <- gtable_add_rows(guides, theme$legend.box.margin[1], pos = 0)
  guides <- gtable_add_rows(guides, theme$legend.box.margin[3], pos = nrow(guides))

  # Add legend box background
  background <- element_grob(theme$legend.box.background %||% element_blank())

  guides <- gtable_add_grob(guides, background, t = 1, l = 1,
    b = -1, r = -1, z = -Inf, clip = "off", name = "legend.box.background")
  guides$name <- "guide-box"
  guides
}

# S3 dispatches

guide_train <- function(...) UseMethod("guide_train")

guide_merge <- function(...) UseMethod("guide_merge")

guide_geom <- function(...) UseMethod("guide_geom")

guide_gengrob <- function(...) UseMethod("guide_gengrob")

# Helpers
matched_aes <- function(layer, guide, defaults) {
  all <- names(c(layer$mapping, if (layer$inherit.aes) defaults, layer$stat$default_aes))
  geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))
  matched <- intersect(intersect(all, geom), names(guide$key))
  matched <- setdiff(matched, names(layer$geom_params))
  setdiff(matched, names(layer$aes_params))
}
