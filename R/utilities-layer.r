# Are integers?
# Determine if a vector contains only integers
# 
# @param vector to test
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
# @param data.frame
# @value data.frame with group variable
# @keyword internal
add_group <- function(data) {
  if (empty(data)) return(zeroGrob())
  
  if (is.null(data$group)) {
    cat <- sapply(data[setdiff(names(data), "label")], is.discrete)
    cat <- intersect(names(which(cat)), .all_aesthetics)
    
    if (length(cat) == 0) {
      data$group <- 1
    } else {
      data$group <- as.numeric(interaction(data[cat]))
    }
  }
  data$group <- as.numeric(factor(data$group, exclude = NULL))
  data
}

# Force matrix
# If not already a matrix, make a 1x1 matrix
# 
# @param object to make into a matrix
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