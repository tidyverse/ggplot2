#' @include ggproto.r
NULL

#' Is this object a facetting specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.facet <- function(x) inherits(x, "Facet")

Facet <- ggproto("Facet", NULL,
  shrink = FALSE,

  # Take input data and define a mapping between facetting variables and ROW,
  # COL and PANEL keys
  #
  # @param data A list of data.frames, the first being the plot data and the
  # subsequent individual layer data
  #
  # @return A data.frame with columns for PANEL, ROW, COL, and facetting vars
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
  # Calls layout and decorate the output with addition information such as
  # and axis information
  #
  # @param data A list of data.frames, the first being the plot data and the
  # subsequent individual layer data
  #
  # @return As $layout but potentially with additional columns depending on
  # subclass implementation
  train = function(self, data) {
    stop("Not implemented", call. = FALSE)
  },
  # Decorate data with PANEL keys based on a layout
  #
  # @param data A list of data.frames, the first being the plot data and the
  # subsequent individual layer data
  #
  # @param layout A layout as returned by $train
  #
  # @return A list of data.frames based on input with a PANEL column mapping
  # data to specific panels. Data can be changed, e.g. if margin=TRUE it will
  # be duplicated
  map = function(self, data, layout) {
    stop("Not implemented", call. = FALSE)
  },
  # Renders the plot area into a gtable
  #
  # @param panel A panel object containing layout, scales and ranges
  #
  # @param coord A Coord object specifying the coordinate system
  #
  # @param theme A theme object
  #
  # @param geom_grobs The generated grobs for each layer as a list, each
  # element split into panels
  #
  # @return A gtable with the central plot area
  render = function(self, panel, coord, theme, geom_grobs) {
    stop("Not implemented", call. = FALSE)
  },
  # Renders the facet strips. This is not a required method as such but used
  # by convention by FacetGrid and FacetWrap. Extensions want necessarly need to
  # implement it
  #
  # @param panel A panel object containing layout, scales and ranges
  #
  # @param theme A theme object
  #
  # @return A list of grobs named after their position. t=top, b=bottom,
  # l=left, r=right
  strips = function(self, panel, theme) {
    stop("Not implemented", call. = FALSE)
  },
  # Renders the plot area(s). This is not a required method as such but used
  # by convention by FacetGrid and FacetWrap. Extensions want necessarly need to
  # implement it
  #
  # @param panel A panel object containing layout, scales and ranges
  #
  # @param coord A Coord object specifying the coordinate system
  #
  # @param theme A theme object
  #
  # @param geom_grobs The generated grobs for each layer as a list, each
  # element split into panels
  #
  # @return Depends on subclass. FacetGrid returns a TableGrob, while FacetWrap
  # returns a lis of gTree
  panels = function(self, panel, coord, theme, geom_grobs) {
    stop("Not implemented", call. = FALSE)
  },
  # Renders the axes for the plot. This is not a required method as such but used
  # by convention by FacetGrid and FacetWrap. Extensions want necessarly need to
  # implement it
  #
  # @param panel A panel object containing layout, scales and ranges
  #
  # @param coord A Coord object specifying the coordinate system
  #
  # @param theme A theme object
  #
  # @param geom_grobs The generated grobs for each layer as a list, each
  # element split into panels
  #
  # @return A list of grobs named after their position. t=top, b=bottom,
  # l=left, r=right
  axes = function(self, panel, coord, theme) {
    stop("Not implemented", call. = FALSE)
  },
  # Create a string representation of the facetting variables
  vars = function(self) {
    stop("Not implemented", call. = FALSE)
  },
  # Prints information of the object, using the $vars method
  print = function(self) {
    cat("<", class(self)[[1]], ">\n", sep = "")
    cat("Variables: ", self$vars(), '\n', sep = "")
  }
)

# Helpers -----------------------------------------------------------------

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
  if (nrow(a) == 0) return(b)
  if (nrow(b) == 0) return(a)

  indexes <- expand.grid(
    i_a = seq_len(nrow(a)),
    i_b = seq_len(nrow(b))
  )
  plyr::unrowname(cbind(
    a[indexes$i_a, , drop = FALSE],
    b[indexes$i_b, , drop = FALSE]
  ))
}

quoted_df <- function(data, vars) {
  values <- plyr::eval.quoted(vars, data, emptyenv(), try = TRUE)
  as.data.frame(compact(values), optional = TRUE, stringsAsFactors = FALSE)
}

layout_null <- function() {
  data.frame(PANEL = 1, ROW = 1, COL = 1)
}
