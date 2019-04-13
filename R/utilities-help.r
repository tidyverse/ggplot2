
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
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
    paste0("\\strong{\\code{", all, "}}"),
    paste0("\\code{", all, "}")
  )
}
