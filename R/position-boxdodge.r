#' Position dodge for box plots
#' 
#' @include position-dodge.r
#' @inheritParams position_dodge
#' @export
position_boxdodge <- function(width = NULL, preserve = c("total", "single")) {
  ggproto(NULL, PositionBoxdodge,
    width = width,
    preserve = match.arg(preserve)
  )  
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionBoxdodge <- ggproto("PositionBoxdodge", PositionDodge,
  
  compute_panel = function(data, params, scales) {
    collide_box(
      data,
      params$width,
      name = "position_boxdodge",
      strategy = pos_boxdodge,
      n = params$n,
      check.width = FALSE
    )
  }
)

pos_boxdodge <- function(df, width, n = NULL) {
  if (is.null(n)) {
    n <- length(unique(df$group))
  }

  if (n == 1)
    return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }
  
  indiv_widths <- df$xmax - df$xmin
  scaled_widths <- indiv_widths / (sum(indiv_widths) / width)

  # Starting xmin value
  x <- unique(df$x)                     # There should just be one x value
  start <- x - width / 2

  divisions <- cumsum(c(start, scaled_widths)) # Divisions between boxes
  df$xmin <- divisions[-length(divisions)]
  df$xmax <- divisions[-1]
  df$x <- rowMeans(df[, c("xmin", "xmax")])    # New x is between xmin and xmax
  
  df
}
