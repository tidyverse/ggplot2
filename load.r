# Load installed package
suppressMessages(library(ggplot2, warn.conflicts = FALSE))

# Find path of this file and source in R files
frame_files <- plyr::compact(plyr::llply(sys.frames(), function(x) x$ofile))
PATH <- dirname(frame_files[[length(frame_files)]])

paths <- dir(file.path(PATH, "R"), full.name=T)
paths <- paths[basename(paths) != "xxx.r"]

# Reorder paths so they're ordered in the same locale as me
loc <- Sys.setlocale("LC_COLLATE", "C")
paths <- paths[order(paths)]
Sys.setlocale("LC_COLLATE", loc)

plyr::l_ply(paths, source)

# Regenerate and load accessors for geoms etc.
accessors_print(file.path(PATH, "R", "xxx.r"))
source(file.path(PATH, "R", "xxx.r"))
source(file.path(PATH, "R", "zxx.r"))


# # Find out whether digest is changing between versions
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
