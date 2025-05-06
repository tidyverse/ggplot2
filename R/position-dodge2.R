#' @export
#' @rdname position_dodge
#' @param padding Padding between elements at the same position. Elements are
#'   shrunk by this proportion to allow space between them. Defaults to 0.1.
position_dodge2 <- function(width = NULL, preserve = "total",
                            padding = 0.1, reverse = FALSE) {
  ggproto(NULL, PositionDodge2,
    width = width,
    preserve = arg_match0(preserve, c("total", "single")),
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
      cli::cli_warn("Width not defined. Set with {.code position_dodge2(width = ...)}")
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
  if (!anyDuplicated(df$xid) > 0) {
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

  start   <- df$xmin
  nonzero <- df$xmax != df$xmin
  missing <- is.na(df$xmin) | is.na(df$xmax)
  start   <- vec_fill_missing(start, "downup")
  end     <- vec_fill_missing(df$xmax, "downup")

  # For end we take largest end seen so far of previous observation
  end <- cummax(c(end[1], end[-nrow(df)]))
  # Start new group when 'start >= end' for non zero-width ranges
  # For zero-width ranges, start must be strictly larger than end
  overlaps <- cumsum(start > end | (start == end & nonzero))
  # Missing ranges always get separate group
  overlaps[missing] <- seq_len(sum(missing)) + max(overlaps, na.rm = TRUE)
  match(overlaps, unique0(overlaps))
}
