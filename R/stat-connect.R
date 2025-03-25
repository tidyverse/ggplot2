#' Connect observations
#'
#' Connect successive points with lines of different shapes.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param connection A specification of how two points are connected. Can be one
#'  of the folloing:
#'  * A string giving a named connection. These options are:
#'      * `"hv"` to first jump horizontally, then vertically.
#'      * `"vh"` to first jump vertically, then horizontally.
#'      * `"mid"` to step half-way between adjacent x-values.
#'      * `"linear"` to use a straight segment.
#'  * A numeric matrix with two columns giving x and y coordinates respectively.
#'    The coordinates should describe points on a path that connect point A
#'    at location (0, 0) and point B at location (1, 1). At least one of these
#'    two points is expected to be included in the coordinates.
#'
#' @eval rd_aesthetics("stat", "connect")
#' @export
#'
#' @examples
#' ggplot(head(economics, 20), aes(date, unemploy)) +
#'   stat_connect(connection = "hv")
#'
#' # Setup custom connections
#' x <- seq(0, 1, length.out = 20)[-1]
#' smooth <- cbind(x, scales::rescale(1 / (1 + exp(-(x * 10 - 5)))))
#' zigzag <- cbind(c(0.4, 0.6, 1), c(0.75, 0.25, 1))
#'
#' ggplot(head(economics, 10), aes(date, unemploy)) +
#'   geom_point() +
#'   stat_connect(aes(colour = "zigzag"), connection = zigzag) +
#'   stat_connect(aes(colour = "smooth"), connection = smooth)
stat_connect <- function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    ...,
    connection = "hv",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatConnect,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      connection = connection,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatConnect <- ggproto(
  "StatConnect", Stat,

  required_aes = c("x|xmin|xmax", "y|ymin|ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(
      data, params,
      range_is_orthogonal = TRUE, ambiguous = TRUE
    )

    connection <- params$connection %||% "hv"

    if (is.character(connection)) {
      check_string(connection)
      connection <- switch(
        arg_match0(connection, c("hv", "vh", "mid", "linear")),
        hv     = matrix(c(1, 1, 0, 1),     2, 2),
        vh     = matrix(c(0, 0, 0, 1),     2, 2),
        mid    = matrix(c(0.5, 0.5, 0, 1), 2, 2),
        linear = matrix(c(0, 1, 0, 1),     2, 2)
      )
    }

    if (!is.matrix(connection) ||
        !typeof(connection) %in% c("integer", "double") ||
        !identical(dim(connection)[2], 2L)) {
      extra <- ""
      if (!is.null(dim(connection)[2])) {
        extra <- paste0(" with ", dim(connection)[2], " column(s)")
      }
      cli::cli_abort(
        "{.arg connection} must be a numeric {.cls matrix} with 2 columns, \\
        not {.obj_type_friendly {connection}}{extra}."
      )
    }

    if (any(!is.finite(connection))) {
      cli::cli_abort(
        "{.arg connection} cannot contain missing or other non-finite values."
      )
    }

    if (nrow(connection) < 1) {
      connection <- NULL
    }

    params$connection <- connection
    params
  },

  compute_group = function(data, scales, connection = "hv", flipped_aes = FALSE) {

    data <- flip_data(data, flipped_aes)

    n <- nrow(data)
    if (n <= 1) {
      return(vec_slice(data, 0))
    }

    if (!is.matrix(connection)) {
      return(data)
    }
    m <- nrow(connection)

    before <- rep(seq_len(n - 1), each = m)
    after  <- rep(seq_len(n)[-1], each = m)

    data <- vec_slice(data, order(data$x %||% data$xmin))

    # Interpolate x
    # Note that `length(x) != length(xjust)`, but these are kept in sync due to
    # the matrix recycling rules (effectively `rep(xjust, ncol(x))`)
    x <- as.matrix(data[intersect(names(data), ggplot_global$x_aes)])
    xjust  <- rep(connection[, 1], n - 1L)
    x <- vec_slice(x, before) * (1 - xjust) + vec_slice(x, after) * xjust

    # Interpolate y
    y <- as.matrix(data[intersect(names(data), ggplot_global$y_aes)])
    yjust  <- rep(connection[, 2], n - 1L)
    y <- vec_slice(y, before) * (1 - yjust) + vec_slice(y, after) * yjust

    # Reconstitute data
    new_data <- vec_slice(data, before)
    new_data[colnames(x)] <- split_matrix(x)
    new_data[colnames(y)] <- split_matrix(y)

    # Esnure data starts and ends are intact
    if (!all(connection[1, ] == c(0, 0))) {
      new_data <- vec_c(vec_slice(data, 1), new_data)
    }
    if (!all(connection[m, ] == c(1, 1))) {
      new_data <- vec_c(new_data, vec_slice(data, n))
    }
    flip_data(new_data, flipped_aes)
  }

)
