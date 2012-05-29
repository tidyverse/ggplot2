aesthetics <- function(x) {
  sort(c(x$required_aes, names(x$default_aes())))
}
geom_aesthetics <- function(x) {
  aesthetics(Geom$find(x))
}
stat_aesthetics <- function(x) {
  aesthetics(Stat$find(x))
}

rd_aesthetics <- function(type, name) {
  obj <- get(firstUpper(type))
  aes <- aesthetics(obj$find(name))
  
  paste("\\code{", type, "_", name, "} ", 
    "understands the following aesthetics:\n\n", 
    "\\itemize{\n",
    paste("  \\item \\code{", aes, "}", collapse = "\n", sep = ""),
    "\n}\n", sep = "")
}