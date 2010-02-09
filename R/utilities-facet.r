# Adding missing levels
# Ensure all data frames in list have same levels for selected variables
# 
# @keyword internal
add_missing_levels <- function(dfs, levels) {
  
  lapply(dfs, function(df) {
    for(var in intersect(names(df), names(levels))) {
      df[var] <- factor(df[, var], levels = ulevels(levels[[var]]))
    }
    df
  })
}

# Unique levels
# Get unique levels of vector
# 
# @keyword internal
ulevels <- function(x) {
  if (is.factor(x)) {
    levels(factor(x))
  } else {
    sort(unique(x))
  }
}