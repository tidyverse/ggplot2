.onAttach <- function(...) {
  withr::with_preserve_seed({
    if (!interactive() || stats::runif(1) > 0.1) return()

    tip <- random_tip()
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
  })
}

random_tip <- function() {
  tips <- c(
    "Posit Community (formerly RStudio Community) is a great place to get help: https://forum.posit.co/c/tidyverse",
    "Learn more about the underlying theory at https://ggplot2-book.org/",
    "Keep up to date with changes at https://tidyverse.org/blog/",
    "Use suppressPackageStartupMessages() to eliminate package startup messages",
    "Need help? Try Stackoverflow: https://stackoverflow.com/tags/ggplot2",
    "Explore the ggplot2 extension gallery to extend your plotting capabilities: https://exts.ggplot2.tidyverse.org/",
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

# In R >= 4.3.0, S7 methods fall back to base Ops behavior when one of the
# arguments is not an S7 object. This ensures compatibility in such cases.
on_load(
  if (getRversion() >= "4.3.0") registerS3method("+", "gg", add_gg)
)

on_load(S7::methods_register())
.onLoad <- function(...) {
  run_on_load()
}

release_questions <- function() {
  c(
    "Have you built the book?"
  )
}
