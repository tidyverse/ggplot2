#' Specify a secondary axis
#'
#' This function is used in conjunction with a position scale to create a
#' secondary axis, positioned opposite of the primary axis. All secondary
#' axes must be based on a one-to-one transformation of the primary axes.
#'
#' @param trans A transformation formula
#'
#' @param name The name of the secondary axis
#'
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the transformation object
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks as output
#'
#' @param labels One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels computed by the transformation object
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - A function that takes the breaks as input and returns labels as output
#'
#' @details
#' `sec_axis` is used to create the specifications for a secondary axis.
#' Except for the `trans` argument any of the arguments can be set to
#' `derive()` which would result in the secondary axis inheriting the
#' settings from the primary axis.
#'
#' `dup_axis` is provide as a shorthand for creating a secondary axis that
#' is a duplication of the primary axis, effectively mirroring the primary axis.
#'
#' @examples
#' p <- ggplot(mtcars, aes(cyl, mpg)) +
#'   geom_point()
#'
#' # Create a simple secondary axis
#' p + scale_y_continuous(sec.axis = sec_axis(~.+10))
#'
#' # Inherit the name from the primary axis
#' p + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(~.+10, name = derive()))
#'
#' # Duplicate the primary axis
#' p + scale_y_continuous(sec.axis = dup_axis())
#'
#' # You can pass in a formula as a shorthand
#' p + scale_y_continuous(sec.axis = ~.^2)
#'
#' # Secondary axes work for date and datetime scales too:
#' df <- data.frame(
#'   dx = seq(as.POSIXct("2012-02-29 12:00:00",
#'                        tz = "UTC",
#'                        format = "%Y-%m-%d %H:%M:%S"
#'   ),
#'   length.out = 10, by = "4 hour"
#'   ),
#'   price = seq(20, 200000, length.out = 10)
#'  )
#'
#' # useful for labelling different time scales in the same plot
#' ggplot(df, aes(x = dx, y = price)) + geom_line() +
#'   scale_x_datetime("Date", date_labels = "%b %d",
#'   date_breaks = "6 hour",
#'   sec.axis = dup_axis(name = "Time of Day",
#'   labels = scales::time_format("%I %p")))
#'
#' # or to transform axes for different timezones
#' ggplot(df, aes(x = dx, y = price)) + geom_line() +
#'   scale_x_datetime("GMT", date_labels = "%b %d %I %p",
#'   sec.axis = sec_axis(~. + 8*3600, name = "GMT+8",
#'   labels = scales::time_format("%b %d %I %p")))
#'
#' @export
sec_axis <- function(trans = NULL, name = waiver(), breaks = waiver(), labels = waiver()) {
  if (!is.formula(trans)) stop("transformation for secondary axes must be a formula", call. = FALSE)
  ggproto(NULL, AxisSecondary,
    trans = trans,
    name = name,
    breaks = breaks,
    labels = labels
  )
}
#' @rdname sec_axis
#'
#' @export
dup_axis <- function(trans = ~., name = derive(), breaks = derive(), labels = derive()) {
  sec_axis(trans, name, breaks, labels)
}

is.sec_axis <- function(x) {
  inherits(x, "AxisSecondary")
}

set_sec_axis <- function(sec.axis, scale) {
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}

#' @rdname sec_axis
#'
#' @export
derive <- function() {
  structure(list(), class = "derived")
}
is.derived <- function(x) {
  inherits(x, "derived")
}
#' @importFrom lazyeval f_eval
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
AxisSecondary <- ggproto("AxisSecondary", NULL,
  trans = NULL,
  axis = NULL,
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),

  # This determines the quality of the remapping from the secondary axis and
  # back to the primary axis i.e. the exactness of the placement of the
  # breakpoints of the secondary axis.
  detail = 1000,

  empty = function(self) {
    is.null(self$trans)
  },

  # Inherit settings from the primary axis/scale
  init = function(self, scale) {
    if (self$empty()) return()
    if (!is.formula(self$trans)) stop("transformation for secondary axes must be a formula", call. = FALSE)
    if (is.derived(self$name) && !is.waive(scale$name)) self$name <- scale$name
    if (is.derived(self$breaks)) self$breaks <- scale$breaks
    if (is.derived(self$labels)) self$labels <- scale$labels
  },

  transform_range = function(self, range) {
    range <- new_data_frame(list("." = range))
    rlang::eval_tidy(
      rlang::f_rhs(self$trans),
      data = range,
      env = rlang::f_env(self$trans)
    )
  },

  break_info = function(self, range, scale) {
    if (self$empty()) return()

    # Get original range before transformation
    inv_range <- scale$trans$inverse(range)

    # Create mapping between primary and secondary range
    old_range <- seq(inv_range[1], inv_range[2], length.out = self$detail)
    full_range <- self$transform_range(old_range)

    # Test for monotonicity
    if (length(unique(sign(diff(full_range)))) != 1)
      stop("transformation for secondary axes must be monotonic")

    # Get break info for the secondary axis
    new_range <- range(scale$transform(full_range), na.rm = TRUE)
    sec_scale <- self$create_scale(new_range, scale)
    range_info <- sec_scale$break_info()
    names(range_info) <- paste0("sec.", names(range_info))
    range_info
  },

  # Temporary scale for the purpose of calling break_info()
  create_scale = function(self, range, primary) {
    scale <- ggproto(NULL, ScaleContinuousPosition,
      name = self$name,
      breaks = self$breaks,
      labels = self$labels,
      limits = range,
      expand = c(0, 0),
      trans = primary$trans
    )
    scale$train(range)
    scale
  },
  make_title = function(title) {
    title
  }
)
