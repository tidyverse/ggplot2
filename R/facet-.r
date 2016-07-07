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
