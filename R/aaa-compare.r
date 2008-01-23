# Functions for comparing images produced by two different versions of ggplot.

a <- "~/Desktop/test-1/"
b <- "~/Desktop/test-2/"

dir_diff <- function(a, b) {
  files_a <- dir(a)
  files_b <- dir(b)
  
  list(
    only_a = setdiff(files_a, files_b),
    only_b = setdiff(files_b, files_a),
    both = intersect(files_a, files_b)
  )
}

compare_img <- function(file, path_a, path_b, path_out) {
  file_a <- file.path(path_a, file)
  file_b <- file.path(path_b, file)

  if (same_file(file_a, file_b)) return(FALSE)

  file_out <- file.path(path_out, file)

  cmd <- paste("compare", file_a, file_b, file_out)
  system(cmd, intern = TRUE)
  TRUE
}

same_file <- function(...) {  
  files <- list(...)
  cmd <- paste("md5 -q", paste(files, collapse=" "))
  length(unique(system(cmd, intern=TRUE))) == 1
}