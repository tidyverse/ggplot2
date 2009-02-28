# Calculate range for discrete position variables
# This is the equivalent of range for discrete variables 
# 
# @keywords internal
discrete_range <- function(..., drop = TRUE) {
  pieces <- list(...)
  
  clevels <- function(x) {
    if (is.null(x)) return(character())
    
    if (is.factor(x)) {
      if (drop) x <- factor(x)
      values <- levels(x)
    } else {
      values <- as.character(unique(x)) 
    }
    if (any(is.na(x))) values <- c(values, NA)
    values
  }
  
  unique(unlist(lapply(pieces, clevels)))
}