.onAttach <- function(...) {
  withr::with_preserve_seed({
    if (!interactive() || stats::runif(1) > 0.1) return()

    tip <- random_tip()
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
  })
}

random_tip <- function() {
  tips <- c(
    "RStudio Community is a great place to get help: https://community.rstudio.com/c/tidyverse",
    "Learn more about the underlying theory at https://ggplot2-book.org/",
    "Keep up to date with changes at https://tidyverse.org/blog/",
    "Use suppressPackageStartupMessages() to eliminate package startup messages",
    "Need help? Try Stackoverflow: https://stackoverflow.com/tags/ggplot2",
    "Need help getting started? Try the R Graphics Cookbook: https://r-graphics.org",
    "Want to understand how all the pieces fit together? Read R for Data Science: https://r4ds.hadley.nz/"
  )

  sample(tips, 1)
}

# To avoid namespace clash with dplyr.
vars <- function(...) {
  quos(...)
}
on_load(
  if (requireNamespace("dplyr", quietly = TRUE)) {
    vars <- dplyr::vars
  }
)
.onLoad <- function(...) {
  run_on_load()
}

release_questions <- function() {
  c(
    "Have you built the book?"
  )
}
