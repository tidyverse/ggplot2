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
  
  df$indiv_width <- df$xmax - df$xmin

  ## How wide is each group?
  group_sizes <- plyr::ddply(df, "x", plyr::summarize, size = sum(indiv_width))

  ## What do we need to multiply each group width by to ensure that the largest is
  ## the target width?
  rescale_factor <- width / max(group_sizes$size)

  ## Multiply by rescale factor
  df$rescaled_width <-  df$indiv_width * rescale_factor

  ## Now what are the total group sizes?
  rescaled_group_sizes <- plyr::ddply(
                                  df,
                                  "x",
                                  plyr::summarize,
                                  size = sum(rescaled_width)
                                )

  ## starting xmin for each group
  starts <- rescaled_group_sizes$x - rescaled_group_sizes$size/2

  for (i in seq_along(starts)) {
    divisions <- cumsum(c(starts[i], df[df$x == i, "rescaled_width"]))
    df[df$x == i, "xmin"] <- divisions[-length(divisions)]
    df[df$x == i, "xmax"] <- divisions[-1]
  }

  df$x <- rowMeans(df[, c("xmin", "xmax")])
  
  df
}
