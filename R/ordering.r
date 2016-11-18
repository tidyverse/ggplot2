# Take data frame `evaled` in layer and convert discrete x or y to a factor
# variable ordered by a numeric variable. The exact method of ordering depends
# on the geom being ordered. E.g., ordering is based on the sum of category
# values for geom_col, but the max for geom_point.
order_data <- function(data, geom, na.rm = TRUE, ...) {
  # Check order exists and is not discrete
  if (is.null(data$order)) return(data)

  if (plyr::is.discrete(data$order)) {
    warning("`order` aesthetic cannot be a discrete variable.")
    return(data)
  }

  # Determine which x and/or y can be ordered
  can_order <- !vapply(data[, c("x", "y")], is.numeric, logical(1))

  if (!any(can_order)) {
    warning("`order` aesthetic requires x and/or y to be non-numeric.")
    return(data)
  }

  can_order <- names(can_order)[can_order]

  # Apply geom-specific ordering
  #
  # There's certainly a way to improve this!
  if (inherits(geom, "GeomCol")) {
    data <- order_by(data, can_order, sum, na.rm = na.rm)
  }
  else if (inherits(geom, "GeomPoint")) {
    data <- order_by(data, can_order, max, na.rm = na.rm)
  }
  else if (inherits(geom, "GeomTile")) {
    data <- order_by(data, can_order, max, na.rm = na.rm)
  }
  else {
    warning("`order` aesthetic not supported for this Geom")
  }

  # Remove order colum as no longer needed
  data$order <- NULL

  data

}

# Take a data frame of columns and a vector of column names that can be ordered
# (`can`). Using the data$order variable to group the other column, order each
# of the columns that can be oredered by apply the function .f to calculate a
# value for each group and then using order. Replace the columns as factors with
# ordered levels.
order_by <- function(data, can, .f, ...) {

  data[can] <- lapply(data[can], function(x) {
    order_as <- order(tapply(data$order, x, .f, ...))
    x <- as.factor(x)
    factor(x, levels = levels(x)[order_as])
  })

  data
}
