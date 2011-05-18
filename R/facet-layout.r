#' Layout panels in a 2d grid.
#' 
#' @params data list of data frames, one for each layer
#' @params rows variables that form the rows
#' @params cols variables that form the columns
#' @return a data frame with columns \code{PANEL}, \code{ROW} and \code{COL},
#'   that match the facetting variable values up with their position in the 
#'   grid
#' @keywords internal
layout_grid <- function(data, rows = NULL, cols = NULL, margins = NULL) {
  if (length(rows) == 0 && length(cols) == 0) return(layout_null())
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)
  
  base <- layout_base(data, c(rows, cols))

  # Add margins
  base <- add_margins(base, names(rows), names(cols), margins)

  # Create panel info dataset
  panel <- id(base)
  panel <- factor(panel, levels = seq_len(attr(panel, "n")))
  
  panels <- cbind(
    PANEL = panel,
    ROW = id(base[names(rows)]) %||% 1,
    COL = id(base[names(cols)]) %||% 1,
    base
  )
  arrange(panels, PANEL)
}

#' Layout out panels in a 1d ribbon.
#'
#' @params drop should missing combinations be excluded from the plot?
#' @keywords internal
layout_wrap <- function(data, vars = NULL, nrow = NULL, ncol = NULL, drop = TRUE) {
  vars <- as.quoted(vars)
  if (length(vars) == 0) return(layout_null())

  base <- layout_base(data, vars)

  id <- id(base, drop = drop)
  n <- attr(id, "n")
  
  dims <- wrap_dims(n, nrow, ncol)
  
  unrowname(data.frame(
    PANEL = factor(id, levels = seq_len(n)),
    ROW = (as.integer(id) - 1L) %/% dims[2] + 1L,
    COL = (as.integer(id) - 1L) %% dims[2] + 1L,
    base
  ))
}

layout_null <- function(data) { 
   data.frame(PANEL = 1, ROW = 1, COL = 1)
}

#' Base layout function that generates all combinations of data needed for
#' facetting
#'
#' @params data list of data frames (one for each layer)
#' @keywords internal
layout_base <- function(data, vars = NULL) {
  if (length(vars) == 0) return(layout_null())

  # For each layer, compute the facet values
  values <- compact(llply(data, quoted_df, vars = vars))

  # Form the base data frame which contains all combinations of facetting
  # variables that appear in the data
  has_all <- unlist(llply(values, length)) == length(vars)
  if (!any(has_all)) {
    stop("At least one layer must contain all variables used for facetting")
  }
  base <- unique(ldply(values[has_all]))
  
  # Systematically add on missing combinations
  for (value in values[!has_all]) {
    if (empty(value)) next;
    
    old <- base[setdiff(names(base), names(value))]
    new <- value[intersect(names(base), names(value))]
    
    base <- rbind(base, expand.grid.df(old, new))
  }
  base
}

quoted_df <- function(data, vars) {
  values <- eval.quoted(vars, data, emptyenv(), try = TRUE)
  as.data.frame(compact(values))
}

# Arrange 1d structure into a grid
wrap_dims <- function(n, nrow = NULL, ncol = NULL) {
    if (is.null(ncol) && is.null(nrow)) {
      rc <- grDevices::n2mfrow(n)
      nrow <- rc[1]
      ncol <- rc[2]
    } else if (is.null(ncol)) {
      ncol <- ceiling(n / nrow)
    } else if (is.null(nrow)) {
      nrow <- ceiling(n / ncol)
    }
    stopifnot(nrow * ncol >= n)
    
    c(nrow, ncol)
}