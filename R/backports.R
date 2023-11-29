# Backport fix from R 3.3:
# https://github.com/wch/r-source/commit/4efc81c98d262f93de9e7911aaa910f5c63cd00f
if (getRversion() < "3.3") {
  absolute.units <- utils::getFromNamespace("absolute.units", "grid")
  absolute.units.unit <- utils::getFromNamespace("absolute.units.unit", "grid")
  absolute.units.unit.list <- utils::getFromNamespace("absolute.units.unit.list", "grid")
  absolute.units.unit.arithmetic <- utils::getFromNamespace("absolute.units.unit.arithmetic", "grid")

  backport_unit_methods <- function() {
    registerS3method("absolute.units", "unit", absolute.units.unit)
    registerS3method("absolute.units", "unit.list", absolute.units.unit.list)
    registerS3method("absolute.units", "unit.arithmetic", absolute.units.unit.arithmetic)
  }
} else {
  backport_unit_methods <- function() {}
}

on_load(backport_unit_methods())

# isFALSE() and isTRUE() are available on R (>=3.5)
if (getRversion() < "3.5") {
  isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x
  isTRUE  <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) &&  x
}

version_unavailable <- function(...) {
  fun <- as_label(current_call()[[1]])
  cli::cli_abort("{.fn {fun}} is not available in R version {getRversion()}.")
}

# Unavailable prior to R 4.1.0
linearGradient <- version_unavailable

on_load({
  if ("linearGradient" %in% getNamespaceExports("grid")) {
    linearGradient <- grid::linearGradient()
  }
})
