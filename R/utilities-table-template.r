layout_col <- function(name, grobs, width = NULL, heights = NULL) {
  width <- width %||% unit(max(unlist(llply(grobs, width_cm))), "cm")
  heights <- heights %||% rep(unit(1, "null"), length(grobs))

  table <- gtable(name = name)
  
  table <- gtable_add_rows(table, heights)
  table <- gtable_add_cols(table, width)
  table <- gtable_add_grob(table, grobs, t = seq_along(grobs), l = 1, 
    clip = "off")
  
  table
}

layout_row <- function(name, grobs, height = NULL, widths = NULL) {
  height <- height %||% unit(max(unlist(llply(grobs, height_cm))), "cm")
  widths <- widths %||% rep(unit(1, "null"), length(grobs))
    
  table <- gtable(name = name)

  table <- gtable_add_cols(table, widths)
  table <- gtable_add_rows(table, height)
  table <- gtable_add_grob(table, grobs, l = seq_along(grobs), t = 1, clip = "off")
  
  table
}

layout_matrix <- function(name, grobs, widths = NULL, heights = NULL, respect = FALSE, clip = "on") {  
  table <- gtable(name = name, respect = respect)

  table <- gtable_add_cols(table, widths)
  table <- gtable_add_rows(table, heights)
  
  table <- gtable_add_grob(table, grobs, t = c(row(grobs)), l = c(col(grobs)),
    clip = clip)

  table
}

layout_empty_row <- function(widths) {
  gtable_add_cols(gtable(), widths)
}

layout_empty_col <- function(heights) {
  gtable_add_rows(gtable(), heights)
}
