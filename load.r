options(Hverbose=FALSE)
loaded <- "ggplot2" %in% search()
suppressMessages(library(ggplot2, warn.conflicts = FALSE))
l(plyr)

paths <- dir("~/documents/ggplot/ggplot/R", full.name=T)
paths <- paths[basename(paths) != "xxx.r"]
l_ply(paths, source)

regen <- function() {
  accessors_print("~/documents/ggplot/ggplot/R/xxx.r")
  source("~/documents/ggplot/ggplot/R/xxx.r")
}
if (!loaded) regen()

# if (!exists("curr")) curr <- NULL
# 
# prev <- curr
# curr <- qplot(mpg, wt, data=mtcars)
# 
# curr_d <- digest.ggplot(curr)
# prev_d <- digest.ggplot(prev)
# 
# if (!is.null(prev) & !identical(curr_d, prev_d)) {
#   curr_b <- bolus(curr)
#   prev_b <- bolus(prev)
#   diff <- sapply(seq_along(curr_b), function(i) !identical(curr_b[[i]], prev_b[[i]]))
#   
#     
#   stop("Digest has changed from ", prev_d, " to ", curr_d, call. = FALSE)
# }
# 
