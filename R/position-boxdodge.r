#' Position dodge for box plots
#'
#' Dodging preserves the vertical position of an geom while adjusting the
#' horizontal position. `position_boxdodge` is a special case of
#' `position_dodge` for arranging box plots, which can have variable widths.
#' 
#' @include position-dodge.r
#' @inheritParams position_dodge
#' @family position adjustments
#' @export
#' @examples
#' ggplot(data = iris, aes(Species, Sepal.Length)) +
#' geom_boxplot(aes(colour = Sepal.Width < 3.2))
#'
#' ggplot(data = iris, aes(Species, Sepal.Length)) +
#' geom_boxplot(aes(colour = Sepal.Width < 3.2), varwidth = TRUE)
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
  
  # Maximum number of boxes that need to be dodged from one another
  nbox <- max(table(df$x))

  # xid represents groups of boxes that share the same x value
  df$xid <- match(df$x, sort(unique(df$x)))
  
  df$new_width <- (df$xmax - df$xmin) / nbox
  df$xmin <- df$x - (df$new_width / 2)
  df$xmax <- df$x + (df$new_width / 2)

  # Find the total width of each group of boxes
  group_sizes <- plyr::ddply(df, "xid", plyr::summarize, size = sum(new_width))
  
  # Starting xmin for each group of boxes
  starts <- group_sizes$xid - (group_sizes$size / 2)

  # Set the boxes in place
  for (i in seq_along(starts)) {
    divisions <- cumsum(c(starts[i], df[df$xid == i, "new_width"]))
    df[df$xid == i, "xmin"] <- divisions[-length(divisions)]
    df[df$xid == i, "xmax"] <- divisions[-1]
  }

  # x values get moved to between xmin and xmax
  df$x <- rowMeans(df[, c("xmin", "xmax")])
  
  df
}
