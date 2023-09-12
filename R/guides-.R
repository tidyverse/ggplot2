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
    if (is.list(args[[1]]) && !inherits(args[[1]], "guide")) args <- args[[1]]
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

update_guides <- function(p, guides) {
  p <- plot_clone(p)
  if (inherits(p$guides, "Guides")) {
    old <- p$guides
    new <- ggproto(NULL, old)
    new$add(guides)
    p$guides <- new
  } else {
    p$guides <- guides
  }
  p
}

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
    if (inherits(guides, "Guides")) {
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
    if (any(is.na(index)) || length(index) == 0) {
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
    if (any(is.na(index)) || length(index) == 0) {
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

  ## Building ------------------------------------------------------------------

  # The `Guides$build()` method is called in ggplotGrob (plot-build.R) and makes
  # the guide box for *non-position* scales.
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
  # 4. Guides$draw()
  #      generate guide grob from each guide object
  #      one guide grob for one guide object
  #
  # 5. Guides$assemble()
  #      arrange all guide grobs

  build = function(self, scales, layers, default_mapping,
                   position, theme, labels) {

    position  <- legend_position(position)
    no_guides <- zeroGrob()
    if (position == "none") {
      return(no_guides)
    }

    theme$legend.key.width  <- theme$legend.key.width  %||% theme$legend.key.size
    theme$legend.key.height <- theme$legend.key.height %||% theme$legend.key.size


    default_direction <- if (position == "inside") "vertical" else position
    theme$legend.box       <- theme$legend.box       %||% default_direction
    theme$legend.direction <- theme$legend.direction %||% default_direction
    theme$legend.box.just  <- theme$legend.box.just  %||% switch(
      position,
      inside     = c("center", "center"),
      vertical   = c("left",   "top"),
      horizontal = c("center", "top")
    )

    # Setup and train on scales
    scales <- scales$non_position_scales()$scales
    if (length(scales) == 0) {
      return(no_guides)
    }
    guides <- self$setup(scales)
    guides$train(scales, theme$legend.direction, labels)
    if (length(guides$guides) == 0) {
      return(no_guides)
    }

    # Merge and process layers
    guides$merge()
    guides$process_layers(layers)
    if (length(guides$guides) == 0) {
      return(no_guides)
    }

    # Draw and assemble
    grobs <- guides$draw(theme)
    guides$assemble(grobs, theme)
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

    if (is.null(aesthetics)) {
      # Aesthetics from scale, as in non-position guides
      aesthetics <- lapply(scales, `[[`, "aesthetics")
      scale_idx  <- rep(seq_along(scales), lengths(aesthetics))
      aesthetics <- unlist(aesthetics, FALSE, FALSE)
    } else {
      # Scale based on aesthetics, as in position guides
      scale_idx  <- seq_along(scales)[match(aesthetics, names(scales))]
    }

    guides <- self$guides

    # For every aesthetic-scale combination, find and validate guide
    new_guides <- lapply(seq_along(scale_idx), function(i) {
      idx <- scale_idx[i]

      # Find guide for aesthetic-scale combination
      # Hierarchy is in the order:
      # plot + guides(XXX) + scale_ZZZ(guide = XXX) > default(i.e., legend)
      guide <- resolve_guide(
        aesthetic = aesthetics[i],
        scale     = scales[[idx]],
        guides    = guides,
        default   = default,
        null      = missing
      )

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
  train = function(self, scales, direction, labels) {

    params <- Map(
      function(guide, param, scale, aes) {
        guide$train(
          param, scale, aes,
          title = labels[[aes]],
          direction = direction
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

    # If there is only one guide, we can exit early, because nothing to merge
    if (length(pairs) == 1) {
      return()
    }

    # The `{order}_{hash}` combination determines groups of guides
    orders <- vapply(self$params, `[[`, 0, "order")
    orders[orders == 0] <- 99
    orders <- sprintf("%02d", orders)
    hashes <- vapply(self$params, `[[`, "", "hash")
    hashes <- paste(orders, hashes, sep = "_")

    # Split by hashes
    indices <- split(seq_along(pairs), hashes)
    indices <- vapply(indices, `[[`, 0L, 1L, USE.NAMES = FALSE) # First index
    groups  <- unname(split(pairs, hashes))
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
  process_layers = function(self, layers) {
    self$params <- Map(
      function(guide, param) guide$get_layer_key(param, layers),
      guide = self$guides,
      param = self$params
    )
    keep <- !vapply(self$params, is.null, logical(1))
    self$subset_guides(keep)
    invisible()
  },

  # Loop over every guide, let them draw their grobs
  draw = function(self, theme) {
    Map(
      function(guide, params) guide$draw(theme, params),
      guide  = self$guides,
      params = self$params
    )
  },

  # Combining multiple guides in a guide box
  assemble = function(grobs, theme) {
    # Set spacing
    theme$legend.spacing   <- theme$legend.spacing    %||% unit(0.5, "lines")
    theme$legend.spacing.y <- theme$legend.spacing.y  %||% theme$legend.spacing
    theme$legend.spacing.x <- theme$legend.spacing.x  %||% theme$legend.spacing

    # Measure guides
    widths  <- lapply(grobs, function(g) sum(g$widths))
    widths  <- inject(unit.c(!!!widths))
    heights <- lapply(grobs, function(g) sum(g$heights))
    heights <- inject(unit.c(!!!heights))

    # Set the justification of each legend within the legend box
    # First value is xjust, second value is yjust
    just <- valid.just(theme$legend.box.just)
    xjust <- just[1]
    yjust <- just[2]

    # setting that is different for vertical and horizontal guide-boxes.
    if (identical(theme$legend.box, "horizontal")) {
      # Set justification for each legend
      for (i in seq_along(grobs)) {
        grobs[[i]] <- editGrob(
          grobs[[i]],
          vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
                        height = heightDetails(grobs[[i]]))
        )
      }

      guides <- gtable_row(name = "guides",
                           grobs = grobs,
                           widths = widths, height = max(heights))

      # add space between the guide-boxes
      guides <- gtable_add_col_space(guides, theme$legend.spacing.x)

    } else { # theme$legend.box == "vertical"
      # Set justification for each legend
      for (i in seq_along(grobs)) {
        grobs[[i]] <- editGrob(
          grobs[[i]],
          vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust),
                        width = widthDetails(grobs[[i]]))
        )
      }

      guides <- gtable_col(name = "guides",
                           grobs = grobs,
                           width = max(widths), heights = heights)

      # add space between the guide-boxes
      guides <- gtable_add_row_space(guides, theme$legend.spacing.y)
    }

    # Add margins around the guide-boxes.
    margin <- theme$legend.box.margin %||% margin()
    guides <- gtable_add_cols(guides, margin[4], pos = 0)
    guides <- gtable_add_cols(guides, margin[2], pos = ncol(guides))
    guides <- gtable_add_rows(guides, margin[1], pos = 0)
    guides <- gtable_add_rows(guides, margin[3], pos = nrow(guides))

    # Add legend box background
    background <- element_grob(theme$legend.box.background %||% element_blank())

    guides <- gtable_add_grob(
      guides, background,
      t = 1, l = 1, b = -1, r = -1,
      z = -Inf, clip = "off",
      name = "legend.box.background"
    )
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

# resolve the guide from the scale and guides
resolve_guide <- function(aesthetic, scale, guides, default = "none", null = "none") {
  guides[[aesthetic]] %||% scale$guide %|W|% default %||% null
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
  if (inherits(guide, "Guide")) {
    return(guide)
  }
  if (inherits(guide, "guide") && is.list(guide)) {
    return(old_guide(guide))
  }
  cli::cli_abort("Unknown guide: {guide}")
}
