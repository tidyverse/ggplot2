aesthetics <- function(x) {
  req_aes <- x$required_aes
  def_aes <- names(x$default_aes)
  def_aes <- setdiff(def_aes, req_aes)
  if (length(req_aes) == 0) {
    # Suppress warnings which occur when sorting NULL
    return(suppressWarnings(sort(names(x$default_aes))))
  }
  if (length(def_aes) == 0) {
    return(paste("\\strong{", sort(x$required_aes), "}",sep = ""))
  }
  return(c(paste("\\strong{", sort(x$required_aes), "}", sep = ""), sort(def_aes)))
}
geom_aesthetics <- function(x) {
  aesthetics(find_subclass("Geom", x))
}
stat_aesthetics <- function(x) {
  aesthetics(find_subclass("Stat", x))
}

rd_aesthetics <- function(type, name) {
  obj <- switch(type,
    geom = find_subclass("Geom", name),
    stat = find_subclass("Stat", name)
  )
  aes <- aesthetics(obj)

  paste("\\code{", type, "_", name, "} ",
    "understands the following aesthetics (required aesthetics are in bold):\n\n",
    "\\itemize{\n",
    paste("  \\item \\code{", aes, "}", collapse = "\n", sep = ""),
    "\n}\n", sep = "")
}
