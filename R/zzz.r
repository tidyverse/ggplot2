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
    "Keep up to date with changes at https://www.tidyverse.org/blog/",
    "Use suppressPackageStartupMessages() to eliminate package startup messages",
    "Need help? Try Stackoverflow: https://stackoverflow.com/tags/ggplot2",
    "Need help getting started? Try the R Graphics Cookbook: https://r-graphics.org",
    "Want to understand how all the pieces fit together? Read R for Data Science: https://r4ds.had.co.nz/"
  )

  sample(tips, 1)
}

# Assigning pathGrob in .onLoad ensures that packages that subclass GeomPolygon
# do not install with error `possible error in 'pathGrob(munched$x, munched$y, ':
# unused argument (pathId = munched$group)` despite the fact that this is correct
# usage
pathGrob <- NULL

.onLoad <- function(...) {
  backport_unit_methods()

  if (getRversion() < as.numeric_version("3.6")) {
    pathGrob <<- function(..., pathId.lengths) {
      grid::pathGrob(...)
    }
  }

  .zeroGrob <<- grob(cl = "zeroGrob", name = "NULL")

  # create the global variables holding all the theme settings
  ggplot_global$theme_all_null <- theme_all_null() # cache all null theme, required by theme_grey()
  ggplot_global$theme_current <- NULL  # the current theme applied to plots if none is specified
  ggplot_global$theme_default <- NULL  # the underlying fallback default theme
  ggplot_global$element_tree <- NULL   # the current element tree for themes
  reset_theme_settings() # sets the preceding three global variables to their actual defaults

  # Used by rbind_dfs
  date <- Sys.Date()
  ggplot_global$date_origin <- date - unclass(date)
  time <- Sys.time()
  ggplot_global$time_origin <- time - unclass(time)

  # To avoid namespace clash with dplyr.
  # It seems surprising that this hack works
  if (requireNamespace("dplyr", quietly = TRUE)) {
    vars <<- dplyr::vars
  }
}

release_questions <- function() {
  c(
    "Have you built the book?"
  )
}
