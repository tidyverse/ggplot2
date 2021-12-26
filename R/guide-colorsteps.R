#' Discretized colourbar guide
#'
#' This guide is version of [guide_colourbar()] for binned colour and fill
#' scales. It shows areas between breaks as a single constant colour instead of
#' the gradient known from the colourbar counterpart.
#'
#' @param even.steps Should the rendered size of the bins be equal, or should
#'   they be proportional to their length in the data space? Defaults to `TRUE`
#' @param show.limits Should labels for the outer limits of the bins be printed?
#'   Default is `NULL` which makes the guide use the setting from the scale
#' @param ticks A logical specifying if tick marks on the colourbar should be
#'   visible.
#' @inheritDotParams guide_colourbar -nbin -raster -ticks -available_aes
#'
#' @inheritSection guide_bins Use with discrete scale
#'
#' @return A guide object
#' @export
#'
#' @family guides
#' @examples
#' df <- expand.grid(X1 = 1:10, X2 = 1:10)
#' df$value <- df$X1 * df$X2
#'
#' p <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#'
#' # Coloursteps guide is the default for binned colour scales
#' p + scale_fill_binned()
#'
#' # By default each bin in the guide is the same size irrespectively of how
#' # their sizes relate in data space
#' p + scale_fill_binned(breaks = c(10, 25, 50))
#'
#' # This can be changed with the `even.steps` argument
#' p + scale_fill_binned(
#'   breaks = c(10, 25, 50),
#'   guide = guide_coloursteps(even.steps = FALSE)
#' )
#'
#' # By default the limits is not shown, but this can be changed
#' p + scale_fill_binned(guide = guide_coloursteps(show.limits = TRUE))
#'
#' # (can also be set in the scale)
#' p + scale_fill_binned(show.limits = TRUE)
#'
guide_coloursteps <- function(even.steps = TRUE, show.limits = NULL, ticks = FALSE, ...) {
  guide <- guide_colourbar(raster = FALSE, ticks = ticks, nbin = 100, ...)
  guide$even.steps <- even.steps
  guide$show.limits <- show.limits
  class(guide) <- c('colorsteps', class(guide))
  guide
}
#' @export
#' @rdname guide_coloursteps
guide_colorsteps <- guide_coloursteps

#' @export
guide_train.colorsteps <- function(guide, scale, aesthetic = NULL) {
  breaks <- scale$get_breaks()
  breaks <- breaks[!is.na(breaks)]
  if (guide$even.steps || !is.numeric(breaks)) {
    if (length(breaks) == 0 || all(is.na(breaks))) {
      return()
    }
    if (is.numeric(breaks)) {
      limits <- scale$get_limits()
      breaks <- breaks[!breaks %in% limits]
      all_breaks <- c(limits[1], breaks, limits[2])
      bin_at <- all_breaks[-1] - diff(all_breaks) / 2
    } else {
      # If the breaks are not numeric it is used with a discrete scale. We check
      # if the breaks follow the allowed format "(<lower>, <upper>]", and if it
      # does we convert it into bin specs
      if (!guide$even.steps) {
        warn("`even.steps = FALSE` is not supported when used together with a discrete scale")
      }
      bin_at <- breaks
      breaks_num <- as.character(breaks)
      breaks_num <- strsplit(gsub("\\(|\\)|\\[|\\]", "", breaks_num), ",\\s?")
      breaks_num <- as.numeric(unlist(breaks_num))
      if (anyNA(breaks_num)) {
        abort('Breaks not formatted correctly for a bin legend. Use `(<lower>, <upper>]` format to indicate bins')
      }
      all_breaks <- breaks_num[c(1, seq_along(breaks) * 2)]
      limits <- all_breaks[c(1, length(all_breaks))]
      breaks <- all_breaks[-c(1, length(all_breaks))]
    }
    ticks <- new_data_frame(setNames(list(scale$map(breaks)), aesthetic %||% scale$aesthetics[1]))
    ticks$.value <- seq_along(breaks) - 0.5
    ticks$.label <- scale$get_labels(breaks)
    guide$nbin <- length(breaks) + 1
    guide$key <- ticks
    guide$bar <- new_data_frame(list(colour = scale$map(bin_at), value = seq_along(bin_at) - 1), n = length(bin_at))

    if (guide$reverse) {
      guide$key <- guide$key[nrow(guide$key):1, ]
      guide$bar <- guide$bar[nrow(guide$bar):1, ]
    }
    guide$hash <- with(guide, digest::digest(list(title, key$.label, bar, name)))
  } else {
    guide <- NextMethod()
    limits <- scale$get_limits()
  }
  if (guide$show.limits %||% scale$show.limits %||% FALSE) {
    edges <- rescale(c(0, 1), to = guide$bar$value[c(1, nrow(guide$bar))], from = c(0.5, guide$nbin - 0.5) / guide$nbin)
    if (guide$reverse) edges <- rev(edges)
    guide$key <- guide$key[c(NA, seq_len(nrow(guide$key)), NA), , drop = FALSE]
    guide$key$.value[c(1, nrow(guide$key))] <- edges
    guide$key$.label[c(1, nrow(guide$key))] <- scale$get_labels(limits)
  }
  guide
}

#' Calculate the default hjust and vjust settings depending on legend
#' direction and position.
#'
#' @noRd
label_just_defaults.colorbar <- function(direction, position) {
  if (direction == "horizontal") {
    switch(
      position,
      "top" = list(hjust = 0.5, vjust = 0),
      list(hjust = 0.5, vjust = 1)
    )
  }
  else {
    switch(
      position,
      "left" = list(hjust = 1, vjust = 0.5),
      list(hjust = 0, vjust = 0.5)
    )
  }
}
