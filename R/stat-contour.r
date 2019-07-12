#' @inheritParams stat_identity
#' @export
#' @eval rd_aesthetics("stat", "contour")
#' @section Computed variables:
#' \describe{
#'  \item{level}{height of contour}
#'  \item{nlevel}{height of contour, scaled to maximum of 1}
#'  \item{piece}{contour piece (an integer)}
#' }
#' @rdname geom_contour
stat_contour <- function(mapping = NULL, data = NULL,
                         geom = "contour", position = "identity",
                         ...,
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
  default_aes = aes(order = stat(level)),

  compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                           breaks = NULL, complete = FALSE, na.rm = FALSE) {
    # If no parameters set, use pretty bins
    if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
      breaks <- pretty(range(data$z), 10)
    }
    # If provided, use bins to calculate binwidth
    if (!is.null(bins)) {
      binwidth <- diff(range(data$z)) / bins
    }
    # If necessary, compute breaks from binwidth
    if (is.null(breaks)) {
      breaks <- fullseq(range(data$z), binwidth)
    }

    if (complete) {
      isobands <- xyz_to_isobands(data, breaks)
      path_df <- iso_to_path(isobands, data$group[1])

      # level here should be a factor
      path_df$level <- factor(path_df$level, levels = names(isobands))

    } else {
      isolines <- xyz_to_isolines(data, breaks)
      path_df <- iso_to_path(isolines, data$group[1])

      # level is expected to be numeric in this case
      path_df$level <- as.numeric(path_df$level)

    }

    path_df$nlevel <- rescale(
      as.numeric(path_df$level),
      to = c(0, 1)
    )

    path_df
  }
)

z_matrix <- function(data) {
  z <- tapply(data$z, data[c("y", "x")], identity)

  if (is.list(z)) {
    stop(
      "Contour requires single `z` at each combination of `x` and `y`.",
      call. = FALSE
    )
  }

  z
}

xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = z_matrix(data),
    levels = breaks
  )
}

xyz_to_isobands <- function(data, breaks) {
  isoband::isobands(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = z_matrix(data),
    levels_low = c(-Inf, breaks),
    levels_high = c(breaks, Inf)
  )
}

iso_to_path <- function(isolines, group = 1) {

  # Convert list of lists into single data frame
  lengths <- vapply(isolines, function(x) length(x$x), integer(1))
  levels <- names(isolines)
  xs <- unlist(lapply(isolines, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(isolines, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(isolines, "[[", "id"), use.names = FALSE)
  pieces <- rep(seq_along(isolines), lengths)

  # Add leading zeros so that groups can be properly sorted later
  groups <- paste(group, sprintf("%03d", pieces), sprintf("%03d", ids), sep = "-")

  new_data_frame(
    list(
      level = rep(levels, lengths),
      x = xs,
      y = ys,
      piece = pieces,
      group = factor(groups)
    ),
    n = length(xs)
  )
}
