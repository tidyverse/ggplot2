#' @include guide-none.R
NULL

#' Set guides for each scale
#'
#' Guides for each scale can be set scale-by-scale with the `guide`
#' argument, or en masse with `guides()`.
#'
#' @param ... List of scale name-guide pairs.  The guide can either
#'   be a string (i.e. "colorbar" or "legend"), or a call to a guide function
#'   (i.e. [guide_colourbar()] or [guide_legend()])
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
#' p <-
#'   ggplot(dat, aes(x, y, colour = p, size = q, shape = r)) +
#'   geom_point()
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
#' p +
#'   guides(
#'     colour = guide_legend("title"),
#'     size = guide_legend("title"),
#'     shape = guide_legend("title")
#'  )
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
  args <- list2(...)
  if (length(args) > 0) {
    if (is.list(args[[1]]) && !is.guide(args[[1]])) args <- args[[1]]
    args <- rename_aes(args)
  }

  idx_false <- vapply(args, isFALSE, FUN.VALUE = logical(1L))
  if (isTRUE(any(idx_false))) {
    deprecate_warn0("3.3.4", "guides(`<scale>` = 'cannot be `FALSE`. Use \"none\" instead')")
    args[idx_false] <- "none"
  }

  # The good path
  if (is_named(args)) {
    return(guides_list(guides = args))
  }

  # If there are no guides, do nothing
  if (length(args) == 0) {
    return(NULL)
  }

  # Raise warning about unnamed guides
  nms <- names(args)
  if (is.null(nms)) {
    msg <- "All guides are unnamed."
  } else {
    unnamed <- which(is.na(nms) | nms == "")
    if (length(unnamed) == length(args)) {
      msg <- "All guides are unnamed."
    } else {
      unnamed <- label_ordinal()(unnamed)
      msg <- "The {.and {unnamed}} guide{?s} {?is/are} unnamed."
    }
  }
  cli::cli_warn(c(
    "Guides provided to {.fun guides} must be named.",
    i = msg
  ))
  NULL
}

#' @export
#' @rdname is_tests
is.guides <- function(x) inherits(x, "Guides")

# Class -------------------------------------------------------------------

# Guides object encapsulates multiple guides and their state.
guides_list <- function(guides = NULL) {
  ggproto(NULL, Guides, guides = guides)
}

Guides <- ggproto(
  "Guides", NULL,

  ## Fields --------------------------------------------------------------------

  # `guides` is the only initially mutable field.
  # It gets populated as a user adds `+ guides(...)` to a plot by the
  # `Guides$add()` method.
  guides = list(),

  # To avoid repeatedly calling `guide_none()` to substitute missing guides,
  # we include its result as a field in the `Guides` class. This field is
  # never updated.
  missing = guide_none(),

  ## Setters -------------------------------------------------------------------

  # Function for adding new guides provided by user
  add = function(self, guides) {
    if (is.null(guides)) {
      return(invisible())
    }
    if (is.guides(guides)) {
      guides <- guides$guides
    }
    self$guides <- defaults(guides, self$guides)
    invisible()
  },

  # Updates the parameters of the guides. NULL parameters indicate switch to
  # `guide_none()` from `Guide$missing` field.
  update_params = function(self, params) {
    if (length(params) != length(self$params)) {
      cli::cli_abort(paste0(
        "Cannot update {length(self$params)} guide{?s} with a list of ",
        "parameter{?s} of length {length(params)}."
      ))
    }
    # Find empty parameters
    is_empty <- vapply(params, is.null, logical(1))
    # Do parameter update
    self$params[!is_empty] <- params[!is_empty]

    # Set empty parameter guides to `guide_none`. Don't overwrite parameters,
    # because things like 'position' are relevant.
    self$guides[is_empty] <- list(self$missing)
    invisible()
  },

  # Function for dropping GuideNone objects from the Guides object. Typically
  # called after training the guides on scales.
  subset_guides = function(self, i) {
    self$guides     <- self$guides[i]
    self$aesthetics <- self$aesthetics[i]
    self$params     <- self$params[i]
    invisible()
  },

  ## Getters -------------------------------------------------------------------

  # Function for retrieving guides by index or aesthetic
  get_guide = function(self, index) {
    if (is.character(index)) {
      index <- match(index, self$aesthetics)
    }
    if (anyNA(index) || length(index) == 0) {
      return(NULL)
    }
    if (length(index) == 1) {
      self$guides[[index]]
    } else {
      self$guides[index]
    }
  },

  # Function for retrieving parameters by guide or aesthetic
  get_params = function(self, index) {
    if (is.character(index)) {
      index <- match(index, self$aesthetics)
    }
    if (anyNA(index) || length(index) == 0) {
      return(NULL)
    }
    if (length(index) == 1) {
      self$params[[index]]
    } else {
      self$params[index]
    }
  },

  get_position = function(self, position) {
    check_string("position")

    guide_positions <- lapply(self$params, `[[`, "position")
    idx <- which(vapply(guide_positions, identical, logical(1), y = position))

    if (length(idx) < 1) {
      # No guide found for position, return missing (guide_none) guide
      return(list(guide = self$missing, params = self$missing$params))
    }
    if (length(idx) == 1) {
      # Happy path when nothing needs to merge
      return(list(guide = self$guides[[idx]], params = self$params[[idx]]))
    }

    # Pair up guides and parameters
    params <- self$params[idx]
    pairs  <- Map(list, guide = self$guides[idx], params = params)

    # Merge pairs sequentially
    order <- order(vapply(params, function(p) as.numeric(p$order), numeric(1)))
    Reduce(
      function(old, new) {
        old$guide$merge(old$params, new$guide, new$params)
      },
      pairs[order]
    )
  },

  get_custom = function(self) {
    custom <- vapply(self$guides, inherits, logical(1), what = "GuideCustom")
    n_custom <- sum(custom)
    if (n_custom < 1) {
      return(guides_list())
    }
    custom <- guides_list(self$guides[custom])
    custom$params <- lapply(custom$guides, `[[`, "params")
    custom$merge()
    custom
  },

  ## Building ------------------------------------------------------------------

  # The `Guides$build()` method is called in ggplot_build (plot-build.R) and
  # collects all information needed from the plot.
  # Note that position scales are handled in `Coord`s, which have their own
  # procedures to do equivalent steps.
  #
  # The procedure is as follows:
  #
  # 1. Guides$setup()
  #      generates a guide object for every scale-aesthetic pair
  #
  # 2. Guides$train()
  #      train each scale and generate guide definition for all guides
  #      here, one guide object for one scale
  #
  # 2. Guides$merge()
  #      merge guide objects if they are overlaid
  #      number of guide objects may be less than number of scales
  #
  # 3. Guides$process_layers()
  #      process layer information and generate geom info.
  #
  # The resulting guide is then drawn in ggplot_gtable

  build = function(self, scales, layers, labels, layer_data, theme) {

    # Empty guides list
    custom <- self$get_custom()
    no_guides <- custom

    # Extract the non-position scales
    scales <- scales$non_position_scales()$scales
    if (length(scales) == 0) {
      return(no_guides)
    }

    # Ensure a 1:1 mapping between aesthetics and scales
    aesthetics <- lapply(scales, `[[`, "aesthetics")
    scales     <- rep.int(scales, lengths(aesthetics))
    aesthetics <- unlist(aesthetics, recursive = FALSE, use.names = FALSE)

    # Setup and train scales
    guides <- self$setup(scales, aesthetics = aesthetics)
    guides$train(scales, labels)

    if (length(guides$guides) == 0) {
      return(no_guides)
    }

    # Merge and process layers
    guides$merge()
    guides$process_layers(layers, layer_data, theme)
    if (length(guides$guides) == 0) {
      return(no_guides)
    }

    guides$guides <- c(guides$guides, custom$guides)
    guides$params <- c(guides$params, custom$params)

    guides
  },

  # Setup routine for resolving and validating guides based on paired scales.
  #
  # The output of the setup is a child `Guides` class with two additional
  # mutable fields, both of which are parallel to the child's `Guides$guides`
  # field.
  #
  # 1. The child's `Guides$params` manages all parameters of a guide that may
  # need to be updated during subsequent steps. This ensures that we never need
  # to update the `Guide` itself and risk reference class shenanigans.
  #
  # 2. The child's `Guides$aesthetics` holds the aesthetic name of the scale
  # that spawned the guide. The `Coord`'s own build methods need this to
  # correctly pick the primary and secondary guides.

  setup = function(
    self, scales, aesthetics = NULL,
    default = self$missing,
    missing = self$missing
  ) {
    guides <- self$guides

    # For every aesthetic-scale combination, find and validate guide
    new_guides <- lapply(seq_along(scales), function(idx) {

      # Find guide for aesthetic-scale combination
      # Hierarchy is in the order:
      # plot + guides(XXX) + scale_ZZZ(guide = XXX) > default(i.e., legend)
      guide <- guides[[aesthetics[idx]]] %||% scales[[idx]]$guide %|W|%
        default %||% missing

      if (isFALSE(guide)) {
        deprecate_warn0("3.3.4", I("The `guide` argument in `scale_*()` cannot be `FALSE`. This "), I('"none"'))
        guide <- "none"
      }

      # Instantiate all guides, e.g. go from "legend" character to
      # GuideLegend class object
      guide <- validate_guide(guide)

      if (inherits(guide, "GuideNone")) {
        return(guide)
      }

      # Check compatibility of scale and guide, e.g. you cannot use GuideAxis
      # to display the "colour" aesthetic.
      scale_aes <- scales[[idx]]$aesthetics
      if (!any(c("x", "y") %in% scale_aes)) scale_aes <- c(scale_aes, "any")
      if (!any(scale_aes %in% guide$available_aes)) {
        warn_aes <- guide$available_aes
        warn_aes[warn_aes == "any"] <- "any non position aesthetic"
        cli::cli_warn(c(
          paste0("{.fn {snake_class(guide)}} cannot be used for ",
                 "{.or {.field {head(scales[[idx]]$aesthetics, 4)}}}."),
          i = "Use {?one of} {.or {.field {warn_aes}}} instead."
        ))
        guide <- missing
      }

      guide
    })

    # Create updated child
    ggproto(
      NULL, self,
      guides     = new_guides,
      # Extract the guide's params to manage separately
      params     = lapply(new_guides, `[[`, "params"),
      aesthetics = aesthetics
    )
  },

  # Loop over every guide-scale combination to perform training
  # A strong assumption here is that `scales` is parallel to the guides
  train = function(self, scales, labels) {

    params <- Map(
      function(guide, param, scale, aes) {
        guide$train(
          param, scale, aes,
          title = labels[[aes]]
        )
      },
      guide = self$guides,
      param = self$params,
      aes   = self$aesthetics,
      scale = scales
    )
    self$update_params(params)
    is_none <- vapply(self$guides, inherits, logical(1), what = "GuideNone")
    self$subset_guides(!is_none)
  },

  # Function to merge guides that encode the same information
  merge = function(self) {
    # Bundle together guides and their parameters
    pairs <- Map(list, guide = self$guides, params = self$params)

    # The `{order}_{hash}` combination determines groups of guides
    orders <- vapply(self$params, `[[`, 0, "order")
    orders[orders == 0] <- 99
    orders <- sprintf("%02d", orders)
    hashes <- vapply(self$params, `[[`, "", "hash")
    hashes <- paste(orders, hashes, sep = "_")

    # If there is only one guide, we can exit early, because nothing to merge
    if (length(pairs) == 1) {
      names(self$guides) <- hashes
      return()
    }

    # Split by hashes
    indices <- split(seq_along(pairs), hashes)
    indices <- vapply(indices, `[[`, 0L, 1L, USE.NAMES = FALSE) # First index
    groups  <- split(pairs, hashes)
    lens    <- lengths(groups)

    # Merge groups with >1 member
    groups[lens > 1] <- lapply(groups[lens > 1], function(group) {
      Reduce(function(old, new) {
        old$guide$merge(old$params, new$guide, new$params)
      }, group)
    })
    groups[lens == 1] <- unlist(groups[lens == 1], FALSE)

    # Update the Guides object
    self$guides <- lapply(groups, `[[`, "guide")
    self$params <- lapply(groups, `[[`, "params")
    self$aesthetics  <- self$aesthetics[indices]
    invisible()
  },

  # Loop over guides to let them extract information from layers
  process_layers = function(self, layers, data = NULL, theme = NULL) {
    self$params <- Map(
      function(guide, param) guide$process_layers(param, layers, data, theme),
      guide = self$guides,
      param = self$params
    )
    keep <- !vapply(self$params, is.null, logical(1))
    self$subset_guides(keep)
    invisible()
  },

  # The `Guides$assemble()` method is called in ggplot_gtable (plot-build.R) and
  # applies the styling from the theme to render each guide and package them
  # into guide boxes.
  #
  # The procedure is as follows
  #
  # 1. Guides$draw()
  #      for every guide object, draw one grob,
  #      then group the grobs in a list per position
  #
  # 2. Guides$package_box()
  #      for every position, collect all individual guides and arrange them
  #      into a guide box which will be inserted into the main gtable
  # Combining multiple guides in a guide box
  assemble = function(self, theme, params = self$params, guides = self$guides) {

    if (length(self$guides) < 1) {
      return(zeroGrob())
    }

    default_position <- theme$legend.position %||% "right"
    if (length(default_position) == 2) {
      default_position <- "inside"
    }
    if (default_position == "none") {
      return(zeroGrob())
    }

    # extract the guide position
    positions <- vapply(
      params,
      function(p) p$position[1] %||% default_position,
      character(1), USE.NAMES = FALSE
    )

    # Populate key sizes
    theme$legend.key.width  <- calc_element("legend.key.width",  theme)
    theme$legend.key.height <- calc_element("legend.key.height", theme)

    grobs <- self$draw(theme, positions, theme$legend.direction)
    keep <- !vapply(grobs, is.zero, logical(1), USE.NAMES = FALSE)
    grobs <- grobs[keep]
    if (length(grobs) < 1) {
      return(zeroGrob())
    }

    # prepare the position of inside legends
    default_inside_just <- calc_element("legend.justification.inside", theme)
    default_inside_position <- calc_element("legend.position.inside", theme)

    groups <- data_frame0(
      positions = positions,
      justs     = list(NULL),
      coords    = list(NULL)
    )

    # we grouped the legends by the positions, for inside legends, they'll be
    # splitted by the actual inside coordinate
    for (i in which(positions == "inside")) {
      # the actual inside position and justification can be set in each guide
      # by `theme` argument, here, we won't use `calc_element()` which will
      # use inherits from `legend.justification` or `legend.position`, we only
      # follow the inside elements from the guide theme
      just <- params[[i]]$theme[["legend.justification.inside"]]
      just <- valid.just(just %||% default_inside_just)
      coord <- params[[i]]$theme[["legend.position.inside"]]
      coord <- coord %||% default_inside_position %||% just

      groups$justs[[i]] <- just
      groups$coord[[i]] <- coord
    }

    groups <- vec_group_loc(vec_slice(groups, keep))
    grobs <- vec_chop(grobs, indices = groups$loc)
    names(grobs) <- groups$key$positions

    # Set spacing
    theme$legend.spacing   <- theme$legend.spacing %||% unit(0.5, "lines")
    theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
    theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)

    # prepare output
    for (i in vec_seq_along(groups)) {
      adjust <- NULL
      position <- groups$key$position[i]
      if (position == "inside") {
        adjust <- theme(
          legend.position.inside = groups$key$coord[[i]],
          legend.justification.inside = groups$key$justs[[i]]
        )
      }
      grobs[[i]] <- self$package_box(grobs[[i]], position, theme + adjust)
    }

    # merge inside grobs into single gtable
    is_inside <- names(grobs) == "inside"
    if (sum(is_inside) > 1) {
      inside <- gtable(unit(1, "npc"), unit(1, "npc"))
      inside <- gtable_add_grob(
        inside, grobs[is_inside],
        t = 1, l = 1, clip = "off",
        name = paste0("guide-box-inside-", seq_len(sum(is_inside)))
      )
      grobs <- grobs[!is_inside]
      grobs$inside <- inside
    }

    # fill in missing guides
    grobs[setdiff(c(.trbl, "inside"), names(grobs))] <- list(zeroGrob())

    grobs
  },

  # Render the guides into grobs
  draw = function(self, theme, positions, direction = NULL,
                  params = self$params,
                  guides = self$guides) {
    directions <- rep(direction %||% "vertical", length(positions))
    if (is.null(direction)) {
      directions[positions %in% c("top", "bottom")] <- "horizontal"
    }

    grobs <- vector("list", length(guides))
    for (i in seq_along(grobs)) {
      grobs[[i]] <- guides[[i]]$draw(
        theme = theme, position = positions[i],
        direction = directions[i], params = params[[i]]
      )
    }
    grobs
  },

  # here, we put `inside_position` and `inside_just` in the last, so that it
  # won't break current implement of patchwork, which depends on the top three
  # arguments to collect guides
  package_box = function(grobs, position, theme) {

    if (is.zero(grobs) || length(grobs) == 0) {
      return(zeroGrob())
    }

    # Determine default direction
    direction <- switch(
      position,
      inside = , left = , right = "vertical",
      top = , bottom = "horizontal"
    )

    # Populate missing theme arguments
    theme$legend.box       <- theme$legend.box       %||% direction
    theme$legend.box.just  <- theme$legend.box.just  %||% switch(
      direction,
      vertical   = c("left", "top"),
      horizontal = c("center", "top")
    )

    # Measure guides
    widths  <- lapply(grobs, `[[`, "widths")
    heights <- lapply(grobs, `[[`, "heights")

    # Check whether legends are stretched in some direction
    stretch_x <- any(unlist(lapply(widths,  unitType)) == "null")
    stretch_y <- any(unlist(lapply(heights, unitType)) == "null")

    # Global justification of the complete legend box
    global_just <- paste0("legend.justification.", position)
    global_just <- valid.just(calc_element(global_just, theme))

    if (position == "inside") {
      # The position of inside legends are set by their justification
      inside_position <- theme$legend.position.inside %||% global_just
      global_xjust  <- inside_position[1]
      global_yjust  <- inside_position[2]
      global_margin <- margin()
    } else {
      global_xjust  <- global_just[1]
      global_yjust  <- global_just[2]
      # Legends to the side of the plot need a margin for justification
      # relative to the plot panel
      global_margin <- margin(
        t = 1 - global_yjust, b = global_yjust,
        r = 1 - global_xjust, l = global_xjust,
        unit = "null"
      )
    }

    # Set the justification of each legend within the legend box
    # First value is xjust, second value is yjust
    box_just  <- valid.just(theme$legend.box.just)
    box_xjust <- box_just[1]
    box_yjust <- box_just[2]

    margin <- calc_element("legend.box.margin", theme) %||% margin()

    # setting that is different for vertical and horizontal guide-boxes.
    if (identical(theme$legend.box, "horizontal")) {
      # Set justification for each legend within the box
      for (i in seq_along(grobs)) {
        grobs[[i]] <- editGrob(
          grobs[[i]],
          vp = viewport(x = box_xjust, y = box_yjust, just = box_just,
                        height = heightDetails(grobs[[i]]))
        )
      }

      spacing <- convertWidth(theme$legend.spacing.x, "cm")
      heights <- unit(height_cm(lapply(heights, sum)), "cm")

      if (stretch_x) {
        widths   <- redistribute_null_units(widths, spacing, margin, "width")
        vp_width <- unit(1, "npc")
      } else {
        widths   <- inject(unit.c(!!!lapply(widths, sum)))
        vp_width <- sum(widths, spacing * (length(grobs) - 1L))
      }

      # Set global justification
      vp <- viewport(
        x = global_xjust, y = global_yjust, just = global_just,
        height = max(heights),
        width  = vp_width
      )

      # Initialise gtable as legends in a row
      guides <- gtable_row(
        name = "guides", grobs = grobs,
        widths = widths, height = max(heights),
        vp = vp
      )

      # Add space between the guide-boxes
      guides <- gtable_add_col_space(guides, spacing)

    } else { # theme$legend.box == "vertical"
      # Set justification for each legend within the box
      for (i in seq_along(grobs)) {
        grobs[[i]] <- editGrob(
          grobs[[i]],
          vp = viewport(x = box_xjust, y = box_yjust, just = box_just,
                        width = widthDetails(grobs[[i]]))
        )
      }

      spacing <- convertHeight(theme$legend.spacing.y, "cm")
      widths  <- unit(width_cm(lapply(widths, sum)), "cm")

      if (stretch_y) {
        heights   <- redistribute_null_units(heights, spacing, margin, "height")
        vp_height <- unit(1, "npc")
      } else {
        heights   <- inject(unit.c(!!!lapply(heights, sum)))
        vp_height <- sum(heights, spacing * (length(grobs) - 1L))
      }

      # Set global justification
      vp <- viewport(
        x = global_xjust, y = global_yjust, just = global_just,
        height = vp_height,
        width =  max(widths)
      )

      # Initialise gtable as legends in a column
      guides <- gtable_col(
        name = "guides", grobs = grobs,
        width = max(widths), heights = heights,
        vp = vp
      )

      # Add space between the guide-boxes
      guides <- gtable_add_row_space(guides, spacing)
    }

    # Add margins around the guide-boxes.
    guides <- gtable_add_padding(guides, margin)

    # Add legend box background
    background <- element_grob(theme$legend.box.background %||% element_blank())

    guides <- gtable_add_grob(
      guides, background,
      t = 1, l = 1, b = -1, r = -1,
      z = -Inf, clip = "off",
      name = "legend.box.background"
    )

    # Set global margin
    if (stretch_x) {
      global_margin[c(2, 4)] <- unit(0, "cm")
    }
    if (stretch_y) {
      global_margin[c(1, 3)] <- unit(0, "cm")
    }
    guides <- gtable_add_padding(guides, global_margin)

    guides$name <- "guide-box"
    guides
  },
  ## Utilities -----------------------------------------------------------------

  print = function(self) {

    guides <- self$guides
    header <- paste0("<Guides[", length(guides), "] ggproto object>\n")

    if (length(guides) == 0) {
      content <- "<empty>"
    } else {
      content <- lapply(guides, function(g) {
        if (is.character(g)) {
          paste0('"', g, '"')
        } else {
          paste0("<", class(g)[[1]], ">")
        }
      })
      nms <- names(content)
      nms <- format(nms, justify = "right")
      content <- unlist(content, FALSE, FALSE)
      content <- format(content, justify = "left")
      content <- paste0(nms, " : ", content)
    }
    cat(c(header, content), sep = "\n")
    invisible(self)
  }
)

# Data accessor -----------------------------------------------------------

#' Extract tick information from guides
#'
#' `get_guide_data()` builds a plot and extracts information from guide keys. This
#' information typically contains positions, values and/or labels, depending
#' on which aesthetic is queried or guide is used.
#'
#' @param plot A `ggplot` or `ggplot_build` object.
#' @param aesthetic A string that describes a single aesthetic for which to
#'   extract guide information. For example: `"colour"`, `"size"`, `"x"` or
#'   `"y.sec"`.
#' @param panel An integer giving a panel number for which to return position guide
#'   information.
#'
#' @return
#' One of the following:
#' * A `data.frame` representing the guide key, when the guide is unique for
#'   the aesthetic.
#' * A `list` when the coord does not support position axes or multiple guides
#'   match the aesthetic.
#' * `NULL` when no guide key could be found.
#' @export
#' @keywords internal
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mtcars) +
#'   aes(mpg, disp, colour = drat, size = drat) +
#'   geom_point() +
#'   facet_wrap(vars(cyl), scales = "free_x")
#'
#' # Guide information for legends
#' get_guide_data(p, "size")
#'
#' # Note that legend guides can be merged
#' merged <- p + guides(colour = "legend")
#' get_guide_data(merged, "size")
#'
#' # Guide information for positions
#' get_guide_data(p, "x", panel = 2)
#'
#' # Coord polar doesn't support proper guides, so we get a list
#' polar <- p + coord_polar()
#' get_guide_data(polar, "theta", panel = 2)
get_guide_data <- function(plot = get_last_plot(), aesthetic, panel = 1L) {

  check_string(aesthetic, allow_empty = FALSE)
  aesthetic <- standardise_aes_names(aesthetic)

  plot <- ggplot_build(plot)

  if (!aesthetic %in% c("x", "y", "x.sec", "y.sec", "theta", "r")) {
    # Non position guides: check if aesthetic in colnames of key
    keys <- lapply(plot$plot$guides$params, `[[`, "key")
    keep <- vapply(keys, function(x) any(colnames(x) %in% aesthetic), logical(1))
    keys <- switch(sum(keep) + 1, NULL, keys[[which(keep)]], keys[keep])
    return(keys)
  }

  # Position guides: find the right layout entry
  check_number_whole(panel)
  layout <- plot$layout$layout
  select <- layout[layout$PANEL == panel, , drop = FALSE]
  if (nrow(select) == 0) {
    return(NULL)
  }
  params <- plot$layout$panel_params[select$PANEL][[1]]

  # If panel params don't have guides, we probably have old coord system
  # that doesn't use the guide system.
  if (is.null(params$guides)) {
    # Old system: just return relevant parameters
    aesthetic <- paste(aesthetic, c("major", "minor", "labels", "range"), sep = ".")
    params <- params[intersect(names(params), aesthetic)]
    return(params)
  } else {
    # Get and return key
    key <- params$guides$get_params(aesthetic)$key
    return(key)
  }
}

# Helpers -----------------------------------------------------------------

matched_aes <- function(layer, guide) {
  all <- names(c(layer$computed_mapping, layer$stat$default_aes))
  geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))

  # Make sure that size guides are shown if a renaming layer is used
  if (layer$geom$rename_size && "size" %in% all && !"linewidth" %in% all) geom <- c(geom, "size")
  matched <- intersect(intersect(all, geom), names(guide$key))
  matched <- setdiff(matched, names(layer$computed_geom_params))
  setdiff(matched, names(layer$aes_params))
}

# This function is used by guides in guide_geom.* to determine whether
# a given layer should be included in the guide
# `matched` is the set of aesthetics that match between the layer and the guide
include_layer_in_guide <- function(layer, matched) {
  if (!is.logical(layer$show.legend)) {
    cli::cli_warn("{.arg show.legend} must be a logical vector.")
    layer$show.legend <- FALSE # save back to layer so we don't issue this warning more than once
    return(FALSE)
  }

  if (length(matched) > 0) {
    # This layer contributes to the legend

    # check if this layer should be included, different behaviour depending on
    # if show.legend is a logical or a named logical vector
    if (is_named(layer$show.legend)) {
      layer$show.legend <- rename_aes(layer$show.legend)
      show_legend <- layer$show.legend[matched]
      # we cannot use `isTRUE(is.na(show_legend))` here because
      # 1. show_legend can be multiple NAs
      # 2. isTRUE() was not tolerant for a named TRUE
      show_legend <- show_legend[!is.na(show_legend)]
      return(length(show_legend) == 0 || any(show_legend))
    }
    return(all(is.na(layer$show.legend)) || isTRUE(layer$show.legend))
  }

  # This layer does not contribute to the legend.
  # Default is to exclude it, except if it is explicitly turned on
  isTRUE(layer$show.legend)
}

# validate guide object
validate_guide <- function(guide) {
  # if guide is specified by character, then find the corresponding guide
  if (is.character(guide)) {
    fun <- find_global(paste0("guide_", guide), env = global_env(),
                       mode = "function")
    if (is.function(fun)) {
      guide <- fun()
    }
  }
  if (is.guide(guide)) {
    return(guide)
  }
  if (inherits(guide, "guide") && is.list(guide)) {
    return(old_guide(guide))
  }
  cli::cli_abort("Unknown guide: {guide}")
}

redistribute_null_units <- function(units, spacing, margin, type = "width") {

  has_null <- vapply(units, function(x) any(unitType(x) == "null"), logical(1))

  # Early exit when we needn't bother with null units
  if (!any(has_null)) {
    units <- lapply(units, sum)
    units <- inject(unit.c(!!!units))
    return(units)
  }

  # Get spacing between guides and margins in absolute units
  size    <- switch(type, width = convertWidth, height = convertHeight)
  spacing <- size(spacing, "cm", valueOnly = TRUE)
  spacing <- sum(rep(spacing, length(units) - 1))
  margin  <- switch(type, width = margin[c(2, 4)], height = margin[c(1, 3)])
  margin  <- sum(size(margin, "cm", valueOnly = TRUE))

  # Get the absolute parts of the unit
  absolute <- vapply(units, function(u) {
    u <- absolute.size(u)
    u <- size(u, "cm", valueOnly = TRUE)
    sum(u)
  }, numeric(1))
  absolute_sum <- sum(absolute) + spacing + margin

  # Get the null parts of the unit
  relative <- rep(0, length(units))
  relative[has_null] <- vapply(units[has_null], function(u) {
    sum(as.numeric(u)[unitType(u) == "null"])
  }, numeric(1))
  relative_sum <- sum(relative)

  if (relative_sum == 0) {
    return(unit(absolute, "cm"))
  }

  relative <- relative / relative_sum
  available_space <- unit(1, "npc") - unit(absolute_sum, "cm")
  relative_space <- available_space * relative
  relative_space + unit(absolute, "cm")
}
