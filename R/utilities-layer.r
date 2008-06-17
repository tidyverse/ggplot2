# Are integers?
# Determine if a vector contains only integers
# 
# @arguments vector to test
# @keyword internal
#X is.integeric(runif(100))
#X is.integeric(rpois(100, 10))
#X is.integeric(1:10)
is.integeric <- function(x) all(floor(x) == x)

# Add group
# Ensure that the data frame contains a grouping variable.
#
# If the \code{group} variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors excluding label.
# 
# @arguments data.frame
# @value data.frame with group variable
# @keyword internal
add_group <- function(data) {
  if (is.null(data$group)) {
    cat <- sapply(data[setdiff(names(data), "label")], is.discrete)
    if (sum(cat) == 0)
      data$group <- 1
    else 
      data$group <- as.numeric(do.call("interaction", data[,cat, drop=FALSE]))
  }
  data$group <- factor(data$group)
  data
}

# Force matrix
# If not already a matrix, make a 1x1 matrix
# 
# @arguments object to make into a matrix
# @keyword internal
force_matrix <- function(x) {
  if (!is.matrix(x)) {
    mat <- list(x)
    dim(mat) <- c(1,1)
    mat
  } else {
    x
  }
}