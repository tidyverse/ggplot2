require(mutatr)

# respect is a layout property
# each grob will occupy it's own viewport -> 
#   just - never matters because will always be an exact size
#   clip
#   layout.pos.row
#   layout.pos.col
TableLayout <- Object$clone()$do({
  self$grobs <- list()
  self$info <- data.frame(
    t = numeric(), r = numeric(), b = numeric(), l = numeric(), 
    clip = character(), name = character(), stringsAsFactors = FALSE)
  self$widths <- list()
  self$heights <- list()
  self$respect <- FALSE
  self$name <- "layout"
  
  self$get_rows <- function() length(self$heights)
  self$get_cols <- function() length(self$widths)
  
  # Find location of a grob
  self$find_location <- function(grob) {
    pos <- unlist(lapply(self$grobs, identical, grob))
    
    self$info[pos, ]
  }
  
  # Add a single grob, possibly spanning multiple rows or columns.
  # 
  # This will not affect height/width
  # 
  # @param grobs a single grob or a list of grobs
  # @param clip should drawing be clipped to the specified cells
  #   (\code{"on"}), the entire table (\code{"inherit"}), or not at all 
  #   (\code{"off"})
  self$add_grob <- function(grobs, t, l, b = t, r = l, clip = "on", 
    name = self$name) 
  {
    if (is.grob(grobs)) grobs <- list(grobs)
    self$grobs <- c(self$grobs, grobs)
    
    info <- data.frame(t = t, l = l, b = b, r = r, clip = clip, name = name,
      stringsAsFactors = FALSE)
    self$info <- rbind(self$info, info)
    
    self
  }
  
  # Add rows and cols  -------------------------------------------------------

  # Add rows to the bottom
  # 
  # @params pos new column will be added below this position. Defaults to
  #   adding row on bottom. \code{0} adds on the top.
  self$add_rows <- function(heights, clip = "inherit", pos = self$rows) {
    stopifnot(length(pos) == 1)
    n <- length(heights)
    
    # Shift existing rows down
    self$heights <- insert.unit(self$heights, heights, pos)
    self$info <- transform(self$info,
      t = ifelse(t > pos, t + n, t),
      b = ifelse(b > pos, b + n, b)
    )

    self
  }
  
  # Add columns to the right
  self$add_cols <- function(widths, clip = "inherit", pos = self$cols) {
    stopifnot(length(pos) == 1)
    n <- length(widths)
    
    # Shift existing columns right
    self$widths <- insert.unit(self$widths, widths, pos)
    self$info <- transform(self$info,
      l = ifelse(l > pos, l + n, l),
      r = ifelse(r > pos, r + n, r)
    )

    self
  }
  
  # Convenience methods for adding empty space -------------------------------
  
  # Add column spacing
  # 
  # @param width either 1 or row - 1 units
  self$add_col_space <- function(width) {
    n <- self$cols - 1
    if (n == 0) return(self)
    
    stopifnot(length(width) == 1 || length(width) == n)
    width <- rep(width, length = n)

    for(i in rev(seq_len(n))) {
      self$add_cols(width[i], pos = i)
    }

    self
  }
  self$add_row_space <- function(height) {
    n <- self$rows - 1
    if (n == 0) return(self)
    
    stopifnot(length(height) == 1 || length(height) == n)
    height <- rep(height, length = n)
    
    for(i in rev(seq_len(n))) {
      self$add_rows(height[i], pos = i)
    }
    
    self
  }

  # Combine with other layouts -----------------------------------------------
  
  self$rbind <- function(other, pos = self$rows) {
    if (other$rows == 0) return(self)
    
    self$heights <- insert.unit(self$heights, other$heights, pos)
    self$grobs <- append(self$grobs, other$grobs)
    
    other_info <- transform(other$info, t = t + pos, b = b + pos)
    self_info <- transform(self$info, 
      t = ifelse(t > pos, t + pos + other$rows, t),
      b = ifelse(b > pos, b + pos + other$rows, b))
    
    self$info <- rbind(self_info, other_info)
    self
  }
  
  self$cbind <- function(other, pos = self$cols) {
    if (other$cols == 0) return(self)
    
    self$widths <- insert.unit(self$widths, other$widths, pos)
    self$grobs <- append(self$grobs, other$grobs)
    
    other_info <- transform(other$info, l = l + pos, r = r + pos)
    self_info <- transform(self$info, 
      l = ifelse(l > pos, l + pos + other$cols, l),
      r = ifelse(r > pos, r + pos + other$cols, r))
    
    self$info <- rbind(self_info, other_info)
    self
  }
  
  # Turn into a grid object --------------------------------------------------
  
  self$layout <- function() {
    grid.layout(
      nrow = self$rows, heights = self$heights,
      ncol = self$cols, widths = self$widths,
      respect = self$respect
    )
  }
  
  self$show_layout <- function() {
    grid.show.layout(self$layout())
  }

  self$viewport <- function() {
    layout_vp <- viewport(layout = self$layout(), name = self$name)
    
    vp <- function(i) {
      with(self$info[i, ], viewport(
        name = paste(name, t, l, sep = "-"), 
        layout.pos.row = c(t), 
        layout.pos.col = c(l), 
        clip = clip
      ))
    }
    children_vp <- do.call("vpList", llply(seq_along(self$grobs), vp))
    vpTree(layout_vp, children_vp)    
  }
  
  self$gList <- function() {
    names <- with(self$info, paste(name, t, l, sep = "-"))

    grobs <- llply(seq_along(names), function(i) {
      editGrob(self$grobs[[i]], vp = vpPath(self$name, names[i]), 
        name = names[i])
    })
    
    do.call("gList", grobs)
  }

  self$gTree <- function() {
    gTree(
      children = self$gList(), 
      childrenvp = self$viewport()
    )
  }
  
  self$draw <- function(new_page = TRUE) {
    if (new_page) grid.newpage()
    grid.draw(self$gTree())
  }
})

insert.unit <- function (x, values, after = length(x)) {
  lengx <- length(x)
  if (lengx == 0) return(values)
  if (length(values) == 0) return(x)
  
  if (after <= 0) {
    unit.c(values, x)
  } else if (after >= lengx) {
    unit.c(x, values)
  } else {
    unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
}

"combine.unit-list" <- function(vectors) {
  do.call("unit.c", vectors)
}