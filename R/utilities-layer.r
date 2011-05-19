# Are integers?
# Determine if a vector contains only integers
# 
# @param vector to test
# @keyword internal
#X is.integeric(runif(100))
#X is.integeric(rpois(100, 10))
#X is.integeric(1:10)
is.integeric <- function(x) all(floor(x) == x)

# Ensure that the data frame contains a grouping variable.
#
# If the \code{group} variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors.
# 
# @param data.frame
# @value data.frame with group variable
# @keyword internal
add_group <- function(data) {
  if (empty(data)) return(data)
  
  if (is.null(data$group)) {
    disc <- vapply(data, is.discrete, logical(1))
    
    if (any(disc)) {
      data$group <- id(data[disc], drop = TRUE)      
    } else {
      data$group <- 1L
    }
  } else {
    data$group <- id(data["group"], drop = TRUE)
  }
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