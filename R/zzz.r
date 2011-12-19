tips <- c(
  "Need help? Try the ggplot2 mailing list: http://groups.google.com/group/ggplot2.",
  paste("Find out what's changed in ggplot2 with\n", 
    "news(Version == \"", packageVersion("ggplot2"), 
    "\", package = \"ggplot2\")", sep = ""),
  "Use suppressPackageStartupMessages to eliminate package startup messages."
)  

.onLoad <- function(...) {
  if (stats::runif(1) > 0.1) return()
  
  tip <- sample(tips, 1)
  packageStartupMessage(tip)
}
