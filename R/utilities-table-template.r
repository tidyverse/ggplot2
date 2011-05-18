layout_col <- function(name, grobs, width = NULL, heights = NULL) {
  width <- width %||% unit(max(unlist(llply(grobs, width_cm))), "cm")
  heights <- heights %||% rep(unit(1, "null"), length(grobs))

  table <- TableLayout$clone()
  table$name <- name
  
  table$add_rows(heights)
  table$add_cols(width)
  table$add_grob(grobs, t = seq_along(grobs), l = 1, clip = "off")
  
  table
}

layout_row <- function(name, grobs, height = NULL, widths = NULL) {
  height <- height %||% unit(max(unlist(llply(grobs, height_cm))), "cm")
  widths <- widths %||% rep(unit(1, "null"), length(grobs))
    
  table <- TableLayout$clone()
  table$name <- name

  table$add_cols(widths)
  table$add_rows(height)
  table$add_grob(grobs, l = seq_along(grobs), t = 1, clip = "off")
  
  table
}

layout_matrix <- function(name, grobs, widths = NULL, heights = NULL, respect = FALSE) {  
  table <- TableLayout$clone()
  table$name <- name

  table$add_cols(widths)
  table$add_rows(heights)
  
  table$add_grob(grobs, t = c(row(grobs)), l = c(col(grobs)), clip = "off")
  table$respect <- respect
  
  table
}

layout_empty_row <- function(widths) {
  table <- TableLayout$clone()
  table$add_cols(widths)
  table
}

layout_empty_col <- function(heights) {
  table <- TableLayout$clone()
  table$add_rows(heights)
  table
}
