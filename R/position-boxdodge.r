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
position_boxdodge <- function(width = NULL, preserve = c("single", "total")) {
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
  preserve = "single",
  setup_params = function(self, data) {
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warning("Width not defined. Set with `position_boxdodge(width = ?)`",
        call. = FALSE)
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      n <- max(table(data$x))
    }

    list(
      width = self$width,
      n = n
    )
  },
                          
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
   
  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }
  
  # xid represents groups of boxes that share the same position
  df$xid <- match(df$x, sort(unique(df$x)))

  if (is.null(n)) {
    # If n is null, preserve total widths of boxes at each position by dividing
    # widths by the number of elements at that position
    n <- table(df$xid)
    df$new_width <- (df$xmax - df$xmin) / n[df$xid]
  } else {
    df$new_width <- (df$xmax - df$xmin) / n
  }
  
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
