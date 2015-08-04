.onAttach <- function(...) {

  # Add snake_case aliases for dot.case formals
  # doing this on attach seems to avoid generating any NOTEs due to missing
  # arguments.
  if (isTRUE(getOption("ggplot.snake_case", FALSE))) {
    alias_to_snake_case("ggplot2")
  }

  if (!interactive() || stats::runif(1) > 0.1) return()

  tips <- c(
    "Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.",
    paste("Find out what's changed in ggplot2 with\n",
      "news(Version == \"", utils::packageVersion("ggplot2"),
      "\", package = \"ggplot2\")", sep = ""),
    "Use suppressPackageStartupMessages to eliminate package startup messages."
  )

  tip <- sample(tips, 1)
  packageStartupMessage(tip)
}
