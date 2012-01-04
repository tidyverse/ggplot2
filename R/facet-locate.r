# Take single layer of data and combine it with panel information to split
# data into different panels.  Adds in extra data for missing facetting
# levels and for margins.
#
# @params data a data frame
locate_grid <- function(data, panels, rows = NULL, cols = NULL, margins = FALSE) {
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)
  vars <- c(names(rows), names(cols))
  
  # Compute facetting values and add margins
  data <- add_margins(data, list(names(rows), names(cols)), margins)
  # Workaround for bug in reshape
  data <- unique(data)
  facet_vals <- quoted_df(data, c(rows, cols))
  values <- compact(llply(data, quoted_df, vars = c(rows, cols)))

  # If any facetting variables are missing, add them in by 
  # duplicating the data
  missing_facets <- setdiff(vars, names(facet_vals))
  if (length(missing_facets) > 0) {
    to_add <- unique(panels[missing_facets])
    
    data_rep <- rep.int(1:nrow(data), nrow(to_add))
    facet_rep <- rep(1:nrow(to_add), each = nrow(data))
    
    data <- unrowname(data[data_rep, , drop = FALSE])
    facet_vals <- unrowname(cbind(
      facet_vals[data_rep, ,  drop = FALSE], 
      to_add[facet_rep, , drop = FALSE]))
  }
  
  # Add PANEL variable
  if (nrow(facet_vals) == 0) {
    # Special case of no facetting
    data$PANEL <- 1
  } else {
    facet_vals[] <- lapply(facet_vals[], as.factor)
    facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
    keys <- join.keys(facet_vals, panels, by = vars)

    data$PANEL <- panels$PANEL[match(keys$x, keys$y)]
  }
  
  arrange(data, PANEL)
}

locate_wrap <- function(data, panels, vars) {
  vars <- as.quoted(vars)
  
  facet_vals <- quoted_df(data, vars)
  facet_vals[] <- lapply(facet_vals[], as.factor)
  
  missing_facets <- setdiff(names(vars), names(facet_vals))
  if (length(missing_facets) > 0) {
    
    to_add <- unique(panels[missing_facets])
    
    data_rep <- rep.int(1:nrow(data), nrow(to_add))
    facet_rep <- rep(1:nrow(to_add), each = nrow(data))
    
    data <- unrowname(data[data_rep, , drop = FALSE])
    facet_vals <- unrowname(cbind(
      facet_vals[data_rep, ,  drop = FALSE], 
      to_add[facet_rep, , drop = FALSE]))
  }
  
  keys <- join.keys(facet_vals, panels, by = names(vars))
  
  data$PANEL <- panels$PANEL[match(keys$x, keys$y)]
  data[order(data$PANEL), ]
}
