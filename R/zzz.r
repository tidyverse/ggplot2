.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()

  tips <- c(
    "Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.",
    "Find out what's changed in ggplot2 at http://github.com/tidyverse/ggplot2/releases.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Stackoverflow is a great place to get help: http://stackoverflow.com/tags/ggplot2.",
    "Need help getting started? Try the cookbook for R: http://www.cookbook-r.com/Graphs/",
    "Want to understand how all the pieces fit together? Buy the ggplot2 book: http://ggplot2.org/book/"
  )

  tip <- sample(tips, 1)
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}

release_questions <- function() {
  c(
    "Have you built the book?"
  )
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ggplot2 <- list(
    ggplot2.continuous.colour = "gradient",
    ggplot2.continuous.fill = "gradient"
  )
  toset <- !(names(op.ggplot2) %in% names(op))
  if(any(toset)) options(op.ggplot2[toset])

  invisible()
}
