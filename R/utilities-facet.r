# Adding missing levels
# Ensure all data frames in list have same levels for selected variables
# 
# @keywords internal
add_missing_levels <- function(dfs, vars) {
  levels <- lapply(vars, function(var) {
    
    values <- unique(unlist(lapply(dfs, function(df) ulevels(df[, var]))))
    if (is.numeric(values)) values <- values[order(values)]
    values
  })
  names(levels) <- vars

  lapply(dfs, function(df) {
    for(var in intersect(names(df), vars)) {
      df[var] <- factor(df[, var], levels = levels[[var]])
    }
    df
  })
}

# Unique levels
# Get unique levels of vector
# 
# @keywords internal
ulevels <- function(x) {
  if (is.factor(x)) {
    levels(factor(x))
  } else {
    sort(unique(x))
  }
}