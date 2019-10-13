
rd_aesthetics <- function(type, name) {
  obj <- switch(type,
    geom = check_subclass(name, "Geom", env = globalenv()),
    stat = check_subclass(name, "Stat", env = globalenv())
  )
  aes <- rd_aesthetics_item(obj)

  c(
    "@section Aesthetics:",
    paste0(
      "\\code{", type, "_", name, "()} ",
      "understands the following aesthetics (required aesthetics are in bold):"
    ),
    "\\itemize{",
    paste0("  \\item ", aes),
    "}",
    "Learn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}."
  )
}

rd_aesthetics_item <- function(x) {
  req <- x$required_aes
  req <- sub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
    paste0("\\strong{\\code{", all, "}}"),
    paste0("\\code{", all, "}")
  )
}

rd_orientation <- function() {
  c(
    "@section Orientation: ",
    paste(
      'This geom treats each axis differently and, thus, can thus have two orientations.',
      'Often the orientation is easy to deduce from a combination of the given',
      'mappings and the types of positional scales in use. Thus, ggplot2 will by',
      'default try to guess which orientation the layer should have. Under rare',
      'circumstances, the orientation is ambiguous and guessing may fail. In that',
      'case the orientation can be specified directly using the \\code{orientation} parameter,',
      'which can be either \\code{"x"} or \\code{"y"}. The value gives the axis that the geom',
      'should run along, \\code{"x"} being the default orientation you would expect for the geom.'
    )
  )
}
