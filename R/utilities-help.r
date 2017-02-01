
rd_aesthetics <- function(type, name) {
  obj <- switch(type,
    geom = find_subclass("Geom", name, globalenv()),
    stat = find_subclass("Stat", name, globalenv())
  )
  aes <- rd_aesthetics_item(obj)

  paste("\\code{", type, "_", name, "} ",
    "understands the following aesthetics (required aesthetics are in bold):\n\n",
    "\\itemize{\n",
    paste("  \\item \\code{", aes, "}", collapse = "\n", sep = ""),
    "\n}\n", sep = "")
}

rd_aesthetics_item <- function(x) {
  req <- x$required_aes
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
    paste0("\\strong{", all, "}"),
    all
  )
}
