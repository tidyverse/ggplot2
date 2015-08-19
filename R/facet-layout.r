# Layout panels in a 2d grid.
#
# @params data list of data frames, one for each layer
# @params rows variables that form the rows
# @params cols variables that form the columns
# @return a data frame with columns \code{PANEL}, \code{ROW} and \code{COL},
#   that match the facetting variable values up with their position in the
#   grid
layout_grid <- function(data, rows = NULL, cols = NULL, margins = NULL,
                       drop = TRUE, as.table = TRUE) {
  if (length(rows) == 0 && length(cols) == 0) return(layout_null())
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)

  base_rows <- layout_base(data, rows, drop = drop)
  if (!as.table) {
    rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
    base_rows[] <- lapply(base_rows, rev_order)
  }
  base_cols <- layout_base(data, cols, drop = drop)
  base <- df.grid(base_rows, base_cols)

  # Add margins
  base <- reshape2::add_margins(base, list(names(rows), names(cols)), margins)
  # Work around bug in reshape2
  base <- unique(base)

  # Create panel info dataset
  panel <- plyr::id(base, drop = TRUE)
  panel <- factor(panel, levels = seq_len(attr(panel, "n")))

  rows <- if (is.null(names(rows))) 1L else plyr::id(base[names(rows)], drop = TRUE)
  cols <- if (is.null(names(cols))) 1L else plyr::id(base[names(cols)], drop = TRUE)

  panels <- data.frame(PANEL = panel, ROW = rows, COL = cols, base,
    check.names = FALSE, stringsAsFactors = FALSE)
  panels <- panels[order(panels$PANEL), , drop = FALSE]
  rownames(panels) <- NULL
  panels
}

# Layout out panels in a 1d ribbon.
#
# @params drop should missing combinations be excluded from the plot?
# @keywords internal
layout_wrap <- function(data, vars = NULL, nrow = NULL, ncol = NULL,
                        as.table = TRUE, drop = TRUE, dir = "h") {
  vars <- as.quoted(vars)
  if (length(vars) == 0) return(layout_null())

  base <- plyr::unrowname(layout_base(data, vars, drop = drop))

  id <- plyr::id(base, drop = TRUE)
  n <- attr(id, "n")

  dims <- wrap_dims(n, nrow, ncol)
  layout <- data.frame(PANEL = factor(id, levels = seq_len(n)))

  if (as.table) {
    layout$ROW <- as.integer((id - 1L) %/% dims[2] + 1L)
  } else {
    layout$ROW <- as.integer(dims[1] - (id - 1L) %/% dims[2])
  }
  layout$COL <- as.integer((id - 1L) %% dims[2] + 1L)

  # For vertical direction, flip row and col
  if (identical(dir, "v")) {
    layout[c("ROW", "COL")] <- layout[c("COL", "ROW")]
  }

  panels <- cbind(layout, plyr::unrowname(base))
  panels <- panels[order(panels$PANEL), , drop = FALSE]
  rownames(panels) <- NULL
  panels
}

layout_null <- function() {
   data.frame(PANEL = 1, ROW = 1, COL = 1)
}

# Base layout function that generates all combinations of data needed for
# facetting
# The first data frame in the list should be the default data for the plot.
# Other data frames in the list are ones that are added to layers.
#
# @params data list of data frames (one for each layer)
# @keywords internal
layout_base <- function(data, vars = NULL, drop = TRUE) {
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
}

ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique(x))
  }
}

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

# Arrange 1d structure into a grid
wrap_dims <- function(n, nrow = NULL, ncol = NULL) {
    if (is.null(ncol) && is.null(nrow)) {
      rc <- grDevices::n2mfrow(n)
      nrow <- rc[2]
      ncol <- rc[1]
    } else if (is.null(ncol)) {
      ncol <- ceiling(n / nrow)
    } else if (is.null(nrow)) {
      nrow <- ceiling(n / ncol)
    }
    stopifnot(nrow * ncol >= n)

    c(nrow, ncol)
}
