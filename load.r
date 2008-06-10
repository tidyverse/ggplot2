options(Hverbose=FALSE)
library(ggplot2)

source.with.err <- function(path) {
  tryCatch(source(path), error = function(x) {print(path); print(x)})
}
lapply(dir("~/documents/ggplot/ggplot/R", full.name=T), source.with.err)

accessors_print("~/documents/ggplot/ggplot/R/xxx.r")
source("~/documents/ggplot/ggplot/R/xxx.r")

if (!exists("curr")) curr <- NULL
prev <- curr
curr <- digest.ggplot(qplot(mpg, wt, data=mtcars))

if (!is.null(prev) & !identical(prev, curr)) {
  stop("Digest has changed from ", prev, " to ", curr, call. = FALSE)
}