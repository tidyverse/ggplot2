#' @include ggproto.r
NULL

#' @section Facets:
#'
#' All \code{facet_*} functions returns a \code{Facet} object or an object of a
#' \code{Facet} subclass. This object describes how to assign data to different
#' panels, how to apply positional scales and how to lay out the panels, once
#' rendered.
#'
#' Extending facets can range from the simple modifications of current facets,
#' to very laborious rewrites with a lot of \code{\link{gtable}} manipulation.
#' For some examples of both, please see the extension vignette.
#'
#' \code{Facet} subclasses, like other extendible ggproto classes, have a range
#' of methods that can be modified. Some of these are required for all new
#' subclasses, while other only need to be modified if need arises.
#'
#' The required methods are:
#'
#' \itemize{
#'   \item \code{compute_layout}: Based on layer data compute a mapping between
#'   panels, axes, and potentially other parameters such as faceting variable
#'   level etc. This method must return a data.frame containing at least the
#'   columns \code{PANEL}, \code{SCALE_X}, and \code{SCALE_Y} each containing
#'   integer keys mapping a PANEL to which axes it should use. In addition the
#'   data.frame can contain whatever other information is necessary to assign
#'   observations to the correct panel as well as determining the position of
#'   the panel.
#'
#'   \item \code{map_data}: This method is supplied the data for each layer in
#'   turn and is expected to supply a \code{PANEL} column mapping each row to a
#'   panel defined in the layout. Additionally this method can also add or
#'   subtract data points as needed e.g. in the case of adding margins to
#'   \code{facet_grid}.
#'
#'   \item \code{draw_panels}: This is where the panels are assembled into a
#'   \code{gtable} object. The method recieves, among others, a list of grobs
#'   defining the content of each panel as generated by the Geoms and Coord
#'   objects. The responsibility of the method is to decorate the panels with
#'   axes and strips as needed, as well as position them relative to each other
#'   in a gtable. For some of the automatic functions to work correctly, each
#'   panel, axis, and strip grob name must be prefixed with "panel", "axis", and
#'   "strip" respectively.
#' }
#'
#' In addition to the methods described above, it is also possible to override
#' the default behaviour of one or more of the following methods:
#'
#' \itemize{
#'   \item \code{setup_params}:
#'   \item \code{init_scales}: Given a master scale for x and y, create panel
#'   specific scales for each panel defined in the layout. The default is to
#'   simply clone the master scale.
#'
#'   \item \code{train_scales}: Based on layer data train each set of panel
#'   scales. The default is to train it on the data related to the panel.
#'
#'   \item \code{finish_data}: Make last-minute modifications to layer data
#'   before it is rendered by the Geoms. The default is to not modify it.
#'
#'   \item \code{draw_back}: Add a grob in between the background defined by the
#'   Coord object (usually the axis grid) and the layer stack. The default is to
#'   return an empty grob for each panel.
#'
#'   \item \code{draw_front}: As above except the returned grob is placed
#'   between the layer stack and the foreground defined by the Coord object
#'   (usually empty). The default is, as above, to return an empty grob.
#'
#'   \item \code{draw_labels}: Given the gtable returned by \code{draw_panels},
#'   add axis titles to the gtable. The default is to add one title at each side
#'   depending on the position and existance of axes.
#' }
#'
#' All extension methods recieve the content of the params field as the params
#' argument, so the constructor function will generally put all relevant
#' information into this field. The only exception is the \code{shrink}
#' parameter which is used to determine if scales are retrained after Stat
#' transformations has been applied.
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Facet <- ggproto("Facet", NULL,
  shrink = FALSE,
  params = list(),

  compute_layout = function(data, params) {
    stop("Not implemented", call. = FALSE)
  },
  map_data = function(data, layout, params) {
    stop("Not implemented", call. = FALSE)
  },
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(x_scale)) {
      scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
    }
    if (!is.null(y_scale)) {
      scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
    }
    scales
  },
  train_scales = function(x_scales, y_scales, layout, data, params) {
    # loop over each layer, training x and y scales in turn
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)

      if (!is.null(x_scales)) {
        x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
        SCALE_X <- layout$SCALE_X[match_id]

        scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales)
      }

      if (!is.null(y_scales)) {
        y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
        SCALE_Y <- layout$SCALE_Y[match_id]

        scale_apply(layer_data, y_vars, "train", SCALE_Y, y_scales)
      }
    }
  },
  draw_back = function(data, layout, x_scales, y_scales, theme, params) {
    rep(list(zeroGrob()), length(unique(layout$PANEL)))
  },
  draw_front = function(data, layout, x_scales, y_scales, theme, params) {
    rep(list(zeroGrob()), length(unique(layout$PANEL)))
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    stop("Not implemented", call. = FALSE)
  },
  draw_labels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels, params) {
    panel_dim <-  find_panel(panels)

    xlab_height_top <- grobHeight(labels$x[[1]])
    panels <- gtable_add_rows(panels, xlab_height_top, pos = 0)
    panels <- gtable_add_grob(panels, labels$x[[1]], name = "xlab-t",
      l = panel_dim$l, r = panel_dim$r, t = 1, clip = "off")

    xlab_height_bottom <- grobHeight(labels$x[[2]])
    panels <- gtable_add_rows(panels, xlab_height_bottom, pos = -1)
    panels <- gtable_add_grob(panels, labels$x[[2]], name = "xlab-b",
      l = panel_dim$l, r = panel_dim$r, t = -1, clip = "off")

    panel_dim <-  find_panel(panels)

    ylab_width_left <- grobWidth(labels$y[[1]])
    panels <- gtable_add_cols(panels, ylab_width_left, pos = 0)
    panels <- gtable_add_grob(panels, labels$y[[1]], name = "ylab-l",
      l = 1, b = panel_dim$b, t = panel_dim$t, clip = "off")

    ylab_width_right <- grobWidth(labels$y[[2]])
    panels <- gtable_add_cols(panels, ylab_width_right, pos = -1)
    panels <- gtable_add_grob(panels, labels$y[[2]], name = "ylab-r",
      l = -1, b = panel_dim$b, t = panel_dim$t, clip = "off")

    panels
  },
  setup_params = function(data, params) {
    params
  },
  setup_data = function(data, params) {
    data
  },
  finish_data = function(data, layout, x_scales, y_scales, params) {
    data
  },
  vars = function() {
    character(0)
  }
)

# Helpers -----------------------------------------------------------------

#' Is this object a facetting specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.facet <- function(x) inherits(x, "Facet")

# A "special" value, currently not used but could be used to determine
# if faceting is active
NO_PANEL <- -1L

unique_combs <- function(df) {
  if (length(df) == 0) return()

  unique_values <- plyr::llply(df, ulevels)
  rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = TRUE))
}

df.grid <- function(a, b) {
  if (is.null(a) || nrow(a) == 0) return(b)
  if (is.null(b) || nrow(b) == 0) return(a)

  indexes <- expand.grid(
    i_a = seq_len(nrow(a)),
    i_b = seq_len(nrow(b))
  )
  plyr::unrowname(cbind(
    a[indexes$i_a, , drop = FALSE],
    b[indexes$i_b, , drop = FALSE]
  ))
}

# When evaluating variables in a facet specification, we evaluate bare
# variables and expressions slightly differently. Bare variables should
# always succeed, even if the variable doesn't exist in the data frame:
# that makes it possible to repeat data across multiple factors. But
# when evaluating an expression, you want to see any errors. That does
# mean you can't have background data when facetting by an expression,
# but that seems like a reasonable tradeoff.
eval_facet_vars <- function(vars, data, env = emptyenv()) {
  nms <- names(vars)
  out <- list()

  for (i in seq_along(vars)) {
    out[[ nms[[i]] ]] <- eval_facet_var(vars[[i]], data, env = env)
  }
  #If facets are like "a~a" or "cyl~cyl"
   if (duplicated(nms)[length(nms)]){
    out[[ paste(names(vars)[-1],c(".1"),sep="") ]] <- eval_facet_var(vars[[i]], data, env = env)
    attributes(out)$duplicated<-TRUE
     }
  tibble::as_tibble(out)
}

eval_facet_var <- function(var, data, env = emptyenv()) {
  if (is.name(var)) {
    var <- as.character(var)
    if (var %in% names(data)) {
      data[[var]]
    } else {
      NULL
    }
  } else if (is.call(var)) {
    eval(var, envir = data, enclos = env)
  } else {
    stop("Must use either variable name or expression when facetting",
      call. = FALSE)
  }
}

layout_null <- function() {
  # PANEL needs to be a factor to be consistent with other facet types
  data.frame(PANEL = factor(1), ROW = 1, COL = 1, SCALE_X = 1, SCALE_Y = 1)
}

check_layout <- function(x) {
  if (all(c("PANEL", "SCALE_X", "SCALE_Y") %in% names(x))) {
    return()
  }

  stop(
    "Facet layout has bad format. ",
    "It must contain columns 'PANEL', 'SCALE_X', and 'SCALE_Y'",
    call. = FALSE
  )
}


#' Get the maximal width/length of a list of grobs
#'
#' @param grobs A list of grobs
#'
#' @return The largest value. measured in cm as a unit object
#'
#' @keywords internal
#' @export
max_height <- function(grobs) {
  unit(max(unlist(lapply(grobs, height_cm))), "cm")
}
#' @rdname max_height
#' @export
max_width <- function(grobs) {
  unit(max(unlist(lapply(grobs, width_cm))), "cm")
}
#' Find panels in a gtable
#'
#' These functions help detect the placement of panels in a gtable, if they are
#' named with "panel" in the beginning. \code{find_panel} returns the extend of
#' the panel area, while \code{panel_cols} and \code{panel_rows} returns the
#' columns and rows that contains panels respectively.
#'
#' @param table A gtable
#'
#' @return A data.frame with some or all of the columns t(op), r(ight),
#' b(ottom), and l(eft)
#'
#' @keywords internal
#' @export
find_panel <- function(table) {
  layout <- table$layout
  panels <- layout[grepl("^panel", layout$name), , drop = FALSE]

  data.frame(
    t = min(panels$t),
    r = max(panels$r),
    b = max(panels$b),
    l = min(panels$l)
  )
}
#' @rdname find_panel
#' @export
panel_cols = function(table) {
  panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  unique(panels[, c('l', 'r')])
}
#' @rdname find_panel
#' @export
panel_rows <- function(table) {
  panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  unique(panels[, c('t', 'b')])
}
#' Take input data and define a mapping between facetting variables and ROW,
#' COL and PANEL keys
#'
#' @param data A list of data.frames, the first being the plot data and the
#' subsequent individual layer data
#' @param env The environment the vars should be evaluated in
#' @param vars A list of quoted symbols matching columns in data
#' @param drop should missing combinations/levels be dropped
#'
#' @return A data.frame with columns for PANEL, ROW, COL, and facetting vars
#'
#' @keywords internal
#' @export
combine_vars <- function(data, env = emptyenv(), vars = NULL, drop = TRUE) {
  if (length(vars) == 0) return(data.frame())

  # For each layer, compute the facet values
  values <- compact(plyr::llply(data, eval_facet_vars, vars = vars, env = env))

  # Form the base data frame which contains all combinations of facetting
  # variables that appear in the data
  has_all <- unlist(plyr::llply(values, length)) == length(vars)
  if (!any(has_all)) {
    stop("At least one layer must contain all variables used for facetting")
  }

  base <- unique(plyr::ldply(values[has_all]))
  if (!drop) {
    base <- unique_combs(base)
  }

  # Systematically add on missing combinations
  for (value in values[!has_all]) {
    if (empty(value)) next;

    old <- base[setdiff(names(base), names(value))]
    new <- unique(value[intersect(names(base), names(value))])
    if (drop) {
      new <- unique_combs(new)
    }
    base <- rbind(base, df.grid(old, new))
  }

  if (empty(base)) {
    stop("Faceting variables must have at least one value", call. = FALSE)
  }

  base
}
#' Render panel axes
#'
#' These helpers facilitates generating theme compliant axes when
#' building up the plot.
#'
#' @param x,y A list of ranges as available to the draw_panel method in
#' \code{Facet} subclasses.
#' @param coord A \code{Coord} object
#' @param theme A \code{theme} object
#' @param transpose Should the output be transposed?
#'
#' @return A list with the element "x" and "y" each containing axis
#' specifications for the ranges passed in. Each axis specification is a list
#' with a "top" and "bottom" element for x-axes and "left" and "right" element
#' for y-axis, holding the respective axis grobs. Depending on the content of x
#' and y some of the grobs might be zeroGrobs. If \code{transpose=TRUE} the
#' content of the x and y elements will be transposed so e.g. all left-axes are
#' collected in a left element as a list of grobs.
#'
#' @keywords internal
#' @export
#'
render_axes <- function(x = NULL, y = NULL, coord, theme, transpose = FALSE) {
  axes <- list()
  if (!is.null(x)) {
    axes$x <- lapply(x, coord$render_axis_h, theme)
  }
  if (!is.null(y)) {
    axes$y <- lapply(y, coord$render_axis_v, theme)
  }
  if (transpose) {
    axes <- list(
      x = list(
        top = lapply(axes$x, `[[`, "top"),
        bottom = lapply(axes$x, `[[`, "bottom")
      ),
      y = list(
        left = lapply(axes$y, `[[`, "left"),
        right = lapply(axes$y, `[[`, "right")
      )
    )
  }
  axes
}
#' Render panel strips
#'
#' All positions are rendered and it is up to the facet to decide which to use
#'
#' @param x,y A data.frame with a column for each variable and a row for each
#' combination to draw
#' @param labeller A labeller function
#' @param theme a \code{theme} object
#'
#' @return A list with an "x" and a "y" element, each containing a "top" and
#' "bottom" or "left" and "right" element respectively. These contains a list of
#' rendered strips as gtables.
#'
#' @keywords internal
#' @export
render_strips <- function(x = NULL, y = NULL, labeller, theme) {
  list(
    x = build_strip(x, labeller, theme, TRUE),
    y = build_strip(y, labeller, theme, FALSE)
  )
}
