.onAttach <- function(...) {
  withr::with_preserve_seed({
    if (!interactive() || stats::runif(1) > 0.1) return()

    tips <- c(
      "RStudio Community is a great place to get help: https://community.rstudio.com/c/tidyverse.",
      "Find out what's changed in ggplot2 at https://github.com/tidyverse/ggplot2/releases.",
      "Use suppressPackageStartupMessages() to eliminate package startup messages.",
      "Need help? Try Stackoverflow: https://stackoverflow.com/tags/ggplot2.",
      "Need help getting started? Try the cookbook for R: http://www.cookbook-r.com/Graphs/",
      "Want to understand how all the pieces fit together? See the R for Data Science book: http://r4ds.had.co.nz/"
    )

    tip <- sample(tips, 1)
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
  })
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

  ## create default theme, store for later use, and set as current theme
  ggplot_global$theme_all_null <- theme_all_null() # cache all null theme, required by theme_grey()
  # the current theme applied to plots if none is specified
  ggplot_global$theme_current <- NULL
  # the underlying fallback default theme
  ggplot_global$theme_default <- NULL
  # both the currently active theme and the default them are created via register_theme_elements()
  register_theme_elements(reset_all = TRUE, modify_current = TRUE)

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
