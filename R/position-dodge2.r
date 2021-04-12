#' @export
#' @rdname position_dodge
#' @param padding Padding between elements at the same position. Elements are
#'   shrunk by this proportion to allow space between them. Defaults to 0.1.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
position_dodge2 <- function(width = NULL, preserve = c("total", "single"),
                            padding = 0.1, reverse = FALSE) {
  ggproto(NULL, PositionDodge2,
    width = width,
    preserve = match.arg(preserve),
    padding = padding,
    reverse = reverse
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodge2 <- ggproto("PositionDodge2", PositionDodge,
  preserve = "total",
  padding = 0.1,
  reverse = FALSE,

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_dodge2(width = ?)`")
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      if ("x" %in% names(data)) {
        # Point geom
        groups <- lapply(panels, function(panel) table(panel$x))
      } else {
        # Interval geom
        groups <- lapply(panels, find_x_overlaps)
      }
      n_groups <- vapply(groups, max, double(1))
      n <- max(n_groups)
    }

    list(
      width = self$width,
      n = n,
      padding = self$padding,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    collided <- collide2(
      data,
      params$width,
      name = "position_dodge2",
      strategy = pos_dodge2,
      n = params$n,
      padding = params$padding,
      check.width = FALSE,
      reverse = params$reverse
    )
    flip_data(collided, params$flipped_aes)
  }
)

pos_dodge2 <- function(df, width, n = NULL, padding = 0.1) {
  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  # xid represents groups of boxes that share the same position
  df$xid <- find_x_overlaps(df)

  # based on xid find newx, i.e. the center of each group of overlapping
  # elements. for boxes, bars, etc. this should be the same as original x, but
  # for arbitrary rects it may not be
  newx <- (tapply(df$xmin, df$xid, min) + tapply(df$xmax, df$xid, max)) / 2
  df$newx <- newx[df$xid]

  if (is.null(n)) {
    # If n is null, preserve total widths of elements at each position by
    # dividing widths by the number of elements at that position
    n <- table(df$xid)
    df$new_width <- (df$xmax - df$xmin) / as.numeric(n[df$xid])
  } else {
    df$new_width <- (df$xmax - df$xmin) / n
  }

  # Find the total width of each group of elements
  group_sizes <- stats::aggregate(
    list(size = df$new_width),
    list(newx = df$newx),
    sum
  )

  # Starting xmin for each group of elements
  starts <- group_sizes$newx - (group_sizes$size / 2)

  # Set the elements in place
  for (i in seq_along(starts)) {
    divisions <- cumsum(c(starts[i], df[df$xid == i, "new_width"]))
    df[df$xid == i, "xmin"] <- divisions[-length(divisions)]
    df[df$xid == i, "xmax"] <- divisions[-1]
  }

  # x values get moved to between xmin and xmax
  df$x <- (df$xmin + df$xmax) / 2

  # If no elements occupy the same position, there is no need to add padding
  if (!any(duplicated(df$xid))) {
    return(df)
  }

  # Shrink elements to add space between them
  df$pad_width <- df$new_width * (1 - padding)
  df$xmin <- df$x - (df$pad_width / 2)
  df$xmax <- df$x + (df$pad_width / 2)

  df$xid <- NULL
  df$newx <- NULL
  df$new_width <- NULL
  df$pad_width <- NULL

  df
}

# Find groups of overlapping elements that need to be dodged from one another
find_x_overlaps <- function(df) {
  overlaps <- numeric(nrow(df))
  overlaps[1] <- counter <- 1

  for (i in seq_asc(2, nrow(df))) {
    if (is.na(df$xmin[i]) || is.na(df$xmax[i - 1]) || df$xmin[i] >= df$xmax[i - 1]) {
      counter <- counter + 1
    }
    overlaps[i] <- counter
  }
  overlaps
}
