

position_connect <- function(connection = "hv") {
  ggproto(
    NULL, PositionConnect,
    connection = validate_connection(connection)
  )
}

PositionConnect <- ggproto(
  "PositionConnect", Position,
  connection = "hv",
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data, ambiguous = TRUE)
    connection <- self$connection
    if (isTRUE(flipped_aes)) {
      connection <- connection[, 2:1]
    }
    list(flipped_aes = flipped_aes, connection = connection)
  },
  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    data <- dapply(data, "group", build_connection, connection = params$connection)
    flip_data(data, params$flipped_aes)
  }
)

validate_connection <- function(connection, call = caller_env()) {
  if (is.character(connection)) {
    check_string(connection)
    connection <- switch(
      arg_match0(connection, c("hv", "vh", "mid", "linear"), error_call = call),
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
      extra <- " with {dim(connection)[2]} columns"
    }
    cli::cli_abort(
      paste0("{.arg connection} must be a numeric {.cls matrix} with 2 columns, \\
      not {.obj_type_friendly {connection}}", extra, "."),
      call = call
    )
  }
  connection
}

build_connection <- function(data, connection) {

  n <- nrow(data)
  if (n <= 1) {
    return(vec_slice(data, 0))
  }
  m <- nrow(connection)

  # Sort data on `x`
  data <- vec_slice(as.data.frame(data), order(data$x %||% data$xmin))

  # Extract x and y aesthetics
  x <- as.matrix(data[intersect(names(data), ggplot_global$x_aes)])
  y <- as.matrix(data[intersect(names(data), ggplot_global$y_aes)])

  # Setup repeats
  before <- rep(seq_len(n - 1), each = m)
  after  <- rep(seq_len(n)[-1], each = m)
  xjust  <- rep(connection[, 1], n - 1L)
  yjust  <- rep(connection[, 2], n - 1L)

  # Do interpolation
  # Note: length(xjust) != length(x). These are kept in sync because the
  # matrix recycling rules effectively do `rep(xjust, ncol(x))`.
  x <- vec_slice(x, before) * (1 - xjust) + vec_slice(x, after) * xjust
  y <- vec_slice(y, before) * (1 - yjust) + vec_slice(y, after) * yjust

  # Reconstitute data
  new_data <- vec_slice(data, before)
  new_data[colnames(x)] <- split_matrix(x)
  new_data[colnames(y)] <- split_matrix(y)

  # Ensure data starts and ends are intact
  if (!all(connection[1, ] == c(0, 0))) {
    new_data <- vec_c(vec_slice(data, 1), new_data)
  }
  if (!all(connection[m, ] == c(1, 1))) {
    new_data <- vec_c(new_data, vec_slice(data, n))
  }
  new_data
}
