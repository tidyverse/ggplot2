#' @include ggproto.r
NULL

#' Facet specification.
#'
#' Create new facetting specification.  For internal use only.
#'
#' @param ... object fields
#' @param shrink shrink scales to fit output of statistics, not raw data
#' @keywords internal
#' @export
facet <- function(..., shrink = TRUE, subclass = c()) {
  structure(list(..., shrink = shrink), class = c(subclass, "facet"))
}

#' Is this object a facetting specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.facet <- function(x) inherits(x, "facet")


# Figure out layout from data from plot and all layers.
#
# This creates the layout data frame which maps from data values to
# panel coordinates: ROW, COL and PANEL. It also records the panels that
# contribute to each x and y scale.
#
# @param data a list of data frames (one for the plot and one for each
#   layer)
facet_train_layout <- function(facet, data)
  UseMethod("facet_train_layout")

facet_map_layout <- function(facet, data, layout)
  UseMethod("facet_map_layout")

facet_render <- function(facet, panels_grob, coord, theme, geom_grobs)
  UseMethod("facet_render")

facet_strips <- function(facet, panel, theme)
  UseMethod("facet_strips")

facet_panels <- function(facet, panel, coord, theme, geom_grobs)
  UseMethod("facet_panels")

facet_axes <- function(facet, panel, coord, theme)
  UseMethod("facet_axes")

# Text description of facetting variables
facet_vars <- function(facet)
  UseMethod("facet_vars")


#' @export
format.facet <- function(x, ...) {
  name <- paste(rev(class(x)), collapse = "_")

  paste(name, "(", facet_vars(x), ")", sep = "")
}

#' @export
print.facet <- function(x, ...) {
  cat(format(x, ...), "\n")
}


# Here comes the ggproto rewrite

Facet <- ggproto("Facet", NULL,
  shrink = FALSE,

  layout = function(data, vars = NULL, drop = TRUE) {
    if (length(vars) == 0) return(data.frame())

    # For each layer, compute the facet values
    values <- compact(plyr::llply(data, quoted_df, vars = vars))

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
  },
  train = function(self, data) {
    stop("Not implemented", call. = FALSE)
  },
  map = function(self, data, layout) {
    stop("Not implemented", call. = FALSE)
  },
  render = function(self, panels_grob, coord, theme, geom_grobs) {
    stop("Not implemented", call. = FALSE)
  },
  strips = function(self, panel, theme) {
    stop("Not implemented", call. = FALSE)
  },
  panels = function(self, panel, coord, theme, geom_grobs) {
    stop("Not implemented", call. = FALSE)
  },
  axes = function(self, panel, coord, theme) {
    stop("Not implemented", call. = FALSE)
  },
  vars = function(self) {
    stop("Not implemented", call. = FALSE)
  },
  print = function(self) {
    cat("<", class(self)[[1]], ">\n", sep = "")
    cat("Variables: ", self$vars(), '\n', sep = "")
  }
)
