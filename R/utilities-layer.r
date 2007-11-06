match.statistic <- function(f) {
  if (is.function(f)) return(f)
  paste("stat", f, sep="_") 
}

compose <- function(fs) {
  if (length(fs) == 1) return(fs) 
  function(data, ...) {
    
    for(f in fs) {
      data <- match.fun(f)(data, ...)
    }
    data
  }
}

addid <- function(data) {
  if (is.null(data$group)) {
    cat <- sapply(data[setdiff(names(data), "label")], is.factor)
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

# Uneval
# Convert an unevaluted list to a list of unevaluated objects
# 
# @arguments unevaluated list (create with substitute)
# @keyword manip 
# @keyword internal
uneval <- function(x) {
  if (length(x) == 1) return(list())
  parts <- vector("list", length(x) - 1)
  names(parts) <- names(x)[-1]
  for(i in length(x):2) parts[[i-1]] <- x[[i]]
  
  parts
}
