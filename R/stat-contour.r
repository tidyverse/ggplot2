#' @inheritParams stat_identity
#' @inheritParams geom_contour
#' @export
#' @eval rd_aesthetics("stat", "contour")
#' @eval rd_aesthetics("stat", "contour_filled")
#' @section Computed variables:
#' The computed variables differ somewhat for contour lines (computed by
#' `stat_contour()`) and contour bands (filled contours, computed by `stat_contour_filled()`).
#' The variables `nlevel` and `piece` are available for both, whereas `level_low`, `level_high`,
#' and `level_mid` are only available for bands. The variable `level` is a numeric or a factor
#' depending on whether lines or bands are calculated.
#' \describe{
#'  \item{`level`}{Height of contour. For contour lines, this is numeric vector that
#'    represents bin boundaries. For contour bands, this is an ordered factor that
#'    represents bin ranges.}
#'  \item{`level_low`, `level_high`, `level_mid`}{(contour bands only) Lower and upper
#'    bin boundaries for each band, as well the mid point between the boundaries.}
#'  \item{`nlevel`}{Height of contour, scaled to maximum of 1.}
#'  \item{`piece`}{Contour piece (an integer).}
#' }
#' @rdname geom_contour
stat_contour <- function(mapping = NULL, data = NULL,
                         geom = "contour", position = "identity",
                         ...,
                         bins = NULL,
                         binwidth = NULL,
                         breaks = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatContour,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_contour
#' @export
stat_contour_filled <- function(mapping = NULL, data = NULL,
                                geom = "contour_filled", position = "identity",
                                ...,
                                bins = NULL,
                                binwidth = NULL,
                                breaks = NULL,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatContourFilled,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatContour <- ggproto("StatContour", Stat,

  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level)),

  setup_params = function(data, params) {
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params
  },

  compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL,
                           breaks = NULL, na.rm = FALSE) {

    breaks <- contour_breaks(z.range, bins, binwidth, breaks)

    isolines <- xyz_to_isolines(data, breaks)
    path_df <- iso_to_path(isolines, data$group[1])

    path_df$level <- as.numeric(path_df$level)
    path_df$nlevel <- rescale_max(path_df$level)

    path_df
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatContourFilled <- ggproto("StatContourFilled", Stat,

  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level), fill = after_stat(level)),

  setup_params = function(data, params) {
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params
  },

  compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE) {
    breaks <- contour_breaks(z.range, bins, binwidth, breaks)

    isobands <- xyz_to_isobands(data, breaks)
    names(isobands) <- pretty_isoband_levels(names(isobands))
    path_df <- iso_to_polygon(isobands, data$group[1])

    path_df$level <- ordered(path_df$level, levels = names(isobands))
    path_df$level_low <- breaks[as.numeric(path_df$level)]
    path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
    path_df$level_mid <- 0.5*(path_df$level_low + path_df$level_high)
    path_df$nlevel <- rescale_max(path_df$level_high)

    path_df
  }
)

#' Calculate the breaks used for contouring
#'
#' @inheritParams geom_contour
#' @param z_range Range of values within which breaks should be calculated
#'
#' @return A vector of breaks
#' @noRd
#'
contour_breaks <- function(z_range, bins = NULL, binwidth = NULL, breaks = NULL) {
  if (!is.null(breaks)) {
    return(breaks)
  }

  # If no parameters set, use pretty bins
  if (is.null(bins) && is.null(binwidth)) {
    breaks <- pretty(z_range, 10)
    return(breaks)
  }

  # If provided, use bins to calculate binwidth
  if (!is.null(bins)) {
    binwidth <- diff(z_range) / (bins - 1)
    breaks <- fullseq(z_range, binwidth)

    # Sometimes the above sequence yields one bin too few.
    # If this happens, try again.
    if (length(breaks) < bins + 1) {
      binwidth <- diff(z_range) / bins
      breaks <- fullseq(z_range, binwidth)
    }

    return(breaks)
  }

  # if we haven't returned yet, compute breaks from binwidth
  fullseq(z_range, binwidth)
}

#' Compute isoband objects
#'
#' @param data A data frame with columns `x`, `y`, and `z`.
#' @param breaks A vector of breaks. These are the values for
#'   which contour lines will be computed.
#'
#' @return An S3 "iso" object, which is a `list()` of `list(x, y, id)`s.
#' @noRd
#'
xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels = breaks
  )
}

xyz_to_isobands <- function(data, breaks) {
  isoband::isobands(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels_low = breaks[-length(breaks)],
    levels_high = breaks[-1]
  )
}

#' Compute input matrix for isoband functions
#'
#' Note that [grDevices::contourLines()] needs transposed
#' output to the matrix returned by this function.
#'
#' @param data A data frame with columns `x`, `y`, and `z`.
#'
#' @return A [matrix()]
#' @noRd
#'
isoband_z_matrix <- function(data) {
  # Convert vector of data to raster
  x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

  nrow <- max(y_pos)
  ncol <- max(x_pos)

  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z

  raster
}

#' Convert the output of isolines functions
#'
#' @param iso the output of [isoband::isolines()]
#' @param group the name of the group
#'
#' @return A data frame that can be passed to [geom_path()].
#' @noRd
#'
iso_to_path <- function(iso, group = 1) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))

  if (all(lengths == 0)) {
    warn("stat_contour(): Zero contours were generated")
    return(new_data_frame())
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so that groups can be properly sorted
  groups <- paste(group, sprintf("%03d", item_id), sprintf("%03d", ids), sep = "-")
  groups <- factor(groups)

  new_data_frame(
    list(
      level = rep(levels, lengths),
      x = xs,
      y = ys,
      piece = as.integer(groups),
      group = groups
    ),
    n = length(xs)
  )
}

#' Convert the output of isoband functions
#'
#' @param iso the output of [isoband::isobands()]
#' @param group the name of the group
#'
#' @return A data frame that can be passed to [geom_polygon()].
#' @noRd
#'
iso_to_polygon <- function(iso, group = 1) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))

  if (all(lengths == 0)) {
    warn("stat_contour(): Zero contours were generated")
    return(new_data_frame())
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so that groups can be properly sorted
  groups <- paste(group, sprintf("%03d", item_id), sep = "-")
  groups <- factor(groups)

  new_data_frame(
    list(
      level = rep(levels, lengths),
      x = xs,
      y = ys,
      piece = as.integer(groups),
      group = groups,
      subgroup = ids
    ),
    n = length(xs)
  )
}

#' Pretty isoband level names
#'
#' @param isoband_levels `names()` of an [isoband::isobands()] object.
#'
#' @return A vector of labels like those used in
#'   [cut()] and [cut_inverval()].
#' @noRd
#'
pretty_isoband_levels <- function(isoband_levels, dig.lab = 3) {
  interval_low <- gsub(":.*$", "", isoband_levels)
  interval_high <- gsub("^[^:]*:", "", isoband_levels)

  label_low <- format(as.numeric(interval_low), digits = dig.lab, trim = TRUE)
  label_high <- format(as.numeric(interval_high), digits = dig.lab, trim = TRUE)

  # from the isoband::isobands() docs:
  # the intervals specifying isobands are closed at their lower boundary
  # and open at their upper boundary
  sprintf("(%s, %s]", label_low, label_high)
}
