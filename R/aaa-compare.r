# Functions for comparing images produced by two different versions of ggplot.

# a <- "~/Desktop/test-1/"
# b <- "~/Desktop/test-2/"

# Compute the set of differences in file make up between two directories.
# 
# @arguments path a
# @arguments path b
# @value list with components only_a, only_b and both
dir_diff <- function(a, b) {
  files_a <- dir(a)
  files_b <- dir(b)
  
  list(
    only_a = setdiff(files_a, files_b),
    only_b = setdiff(files_b, files_a),
    both = intersect(files_a, files_b)
  )
}

# Compare two images
# Saves image displaying differences
# 
# @arguments name of file
# @arguments location of image a
# @arguments location of image b
# @arguments location where output should be saved
# @keyword internal
compare_img <- function(file, path_a, path_b, path_out) {
  file_a <- file.path(path_a, file)
  file_b <- file.path(path_b, file)

  if (same_file(file_a, file_b)) return(FALSE)

  file_out <- file.path(path_out, file)

  cmd <- paste("compare", file_a, file_b, file_out)
  system(cmd, intern = TRUE)
  TRUE
}

# Test if all files are the same
# Uses md5 checksum to rapidly check if multiple files are equal. 
# 
# @arguments character vector of paths
# @value boolean
same_file <- function(...) {  
  files <- list(...)
  cmd <- paste("md5 -q", paste(files, collapse=" "))
  length(unique(system(cmd, intern=TRUE))) == 1
}