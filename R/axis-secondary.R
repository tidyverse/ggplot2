#' Specify a secondary axis
#'
#' This function is used in conjunction with a position scale to create a
#' secondary axis, positioned opposite of the primary axis. All secondary
#' axes must be based on a one-to-one transformation of the primary axes.
#'
#' @param trans A formula or function of transformation
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
#' @param guide A position guide that will be used to render
#'   the axis on the plot. Usually this is [guide_axis()].
#'
#' @details
#' `sec_axis()` is used to create the specifications for a secondary axis.
#' Except for the `trans` argument any of the arguments can be set to
#' `derive()` which would result in the secondary axis inheriting the
#' settings from the primary axis.
#'
#' `dup_axis()` is provide as a shorthand for creating a secondary axis that
#' is a duplication of the primary axis, effectively mirroring the primary axis.
#'
#' As of v3.1, date and datetime scales have limited secondary axis capabilities.
#' Unlike other continuous scales, secondary axis transformations for date and datetime scales
#' must respect their primary POSIX data structure.
#' This means they may only be transformed via addition or subtraction, e.g.
#' `~ . + hms::hms(days = 8)`, or
#' `~ . - 8*60*60`. Nonlinear transformations will return an error.
#' To produce a time-since-event secondary axis in this context, users
#' may consider adapting secondary axis labels.
#'
#' @examples
#' p <- ggplot(mtcars, aes(cyl, mpg)) +
#'   geom_point()
#'
#' # Create a simple secondary axis
#' p + scale_y_continuous(sec.axis = sec_axis(~ . + 10))
#'
#' # Inherit the name from the primary axis
#' p + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(~ . + 10, name = derive()))
#'
#' # Duplicate the primary axis
#' p + scale_y_continuous(sec.axis = dup_axis())
#'
#' # You can pass in a formula as a shorthand
#' p + scale_y_continuous(sec.axis = ~ .^2)
#'
#' # Secondary axes work for date and datetime scales too:
#' df <- data.frame(
#'   dx = seq(
#'     as.POSIXct("2012-02-29 12:00:00", tz = "UTC"),
#'     length.out = 10,
#'     by = "4 hour"
#'   ),
#'   price = seq(20, 200000, length.out = 10)
#'  )
#'
#' # This may useful for labelling different time scales in the same plot
#' ggplot(df, aes(x = dx, y = price)) +
#'   geom_line() +
#'   scale_x_datetime(
#'     "Date",
#'     date_labels = "%b %d",
#'     date_breaks = "6 hour",
#'     sec.axis = dup_axis(
#'       name = "Time of Day",
#'       labels = scales::time_format("%I %p")
#'     )
#'   )
#'
#' # or to transform axes for different timezones
#' ggplot(df, aes(x = dx, y = price)) +
#'   geom_line() +
#'   scale_x_datetime("
#'     GMT",
#'     date_labels = "%b %d %I %p",
#'     sec.axis = sec_axis(
#'       ~ . + 8 * 3600,
#'       name = "GMT+8",
#'       labels = scales::time_format("%b %d %I %p")
#'     )
#'   )
#'
#' @export
sec_axis <- function(trans = NULL, name = waiver(), breaks = waiver(), labels = waiver(),
                     guide = waiver()) {
  # sec_axis() historically accpeted two-sided formula, so be permissive.
  if (length(trans) > 2) trans <- trans[c(1,3)]

  trans <- as_function(trans)
  ggproto(NULL, AxisSecondary,
    trans = trans,
    name = name,
    breaks = breaks,
    labels = labels,
    guide = guide
  )
}
#' @rdname sec_axis
#'
#' @export
dup_axis <- function(trans = ~., name = derive(), breaks = derive(), labels = derive(), guide = derive()) {
  sec_axis(trans, name, breaks, labels, guide)
}

is.sec_axis <- function(x) {
  inherits(x, "AxisSecondary")
}

set_sec_axis <- function(sec.axis, scale) {
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) {
      cli::cli_abort("Secondary axes must be specified using {.fn sec_axis}")
    }
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
    if (self$empty()) {
      return()
    }
    if (!is.function(self$trans)) {
      cli::cli_abort("Transformation for secondary axes must be a function")
    }
    if (is.derived(self$name) && !is.waive(scale$name)) self$name <- scale$name
    if (is.derived(self$breaks)) self$breaks <- scale$breaks
    if (is.waive(self$breaks)) self$breaks <- scale$trans$breaks
    if (is.derived(self$labels)) self$labels <- scale$labels
    if (is.derived(self$guide)) self$guide <- scale$guide
  },

  transform_range = function(self, range) {
    self$trans(range)
  },

  mono_test = function(self, scale){
    range <- scale$range$range

    # Check if plot is empty
    if (is.null(range)) {
      return()
    }

    along_range <- seq(range[1], range[2], length.out = self$detail)
    old_range <- scale$trans$inverse(along_range)

    # Create mapping between primary and secondary range
    full_range <- self$transform_range(old_range)

    # Test for monotonicity
    if (!is_unique(sign(diff(full_range))))
      cli::cli_abort("Transformation for secondary axes must be monotonic")
  },

  break_info = function(self, range, scale) {
    if (self$empty()) return()

    # Test for monotonicity on unexpanded range
    self$mono_test(scale)

    # Get scale's original range before transformation
    along_range <- seq(range[1], range[2], length.out = self$detail)
    old_range <- scale$trans$inverse(along_range)

    # Create mapping between primary and secondary range
    full_range <- self$transform_range(old_range)

    # Remove duplicates in the expanded area of the range that can arise if
    # the transformation is non-monotonic in the expansion. The split ensures
    # the middle duplicated are kept
    duplicates <- c(
      !duplicated(full_range[seq_len(self$detail/2)], fromLast = TRUE),
      !duplicated(full_range[-seq_len(self$detail/2)])
    )
    old_range <- old_range[duplicates]
    full_range <- full_range[duplicates]

    # Get break info for the secondary axis
    new_range <- range(full_range, na.rm = TRUE)

    # patch for date and datetime scales just to maintain functionality
    # works only for linear secondary transforms that respect the time or date transform
    if (scale$trans$name %in% c("date", "time")) {
      temp_scale <- self$create_scale(new_range, trans = scale$trans)
      range_info <- temp_scale$break_info()
      old_val_trans <- rescale(range_info$major, from = c(0, 1), to = range)
      old_val_minor_trans <- rescale(range_info$minor, from = c(0, 1), to = range)
    } else {
      temp_scale <- self$create_scale(new_range)
      range_info <- temp_scale$break_info()

      # Map the break values back to their correct position on the primary scale
      if (!is.null(range_info$major_source)) {
        old_val <- approx(full_range, old_range, range_info$major_source)$y
        old_val_trans <- scale$trans$transform(old_val)

        # rescale values from 0 to 1
        range_info$major[] <- round(
          rescale(
            scale$map(old_val_trans, range(old_val_trans)),
            from = range
          ),
          digits = 3
        )
      } else {
        old_val_trans <- NULL
      }

      if (!is.null(range_info$minor_source)) {
        old_val_minor <- approx(full_range, old_range, range_info$minor_source)$y
        old_val_minor_trans <- scale$trans$transform(old_val_minor)

        range_info$minor[] <- round(
          rescale(
            scale$map(old_val_minor_trans, range(old_val_minor_trans)),
            from = range
          ),
          digits = 3
        )
      } else {
        old_val_minor_trans <- NULL
      }
    }

    # The _source values should be in (primary) scale_transformed space,
    # so that the coord doesn't have to know about the secondary scale transformation
    # when drawing the axis. The values in user space are useful for testing.
    range_info$major_source_user <- range_info$major_source
    range_info$minor_source_user <- range_info$minor_source
    range_info$major_source[] <- old_val_trans
    range_info$minor_source[] <- old_val_minor_trans

    names(range_info) <- paste0("sec.", names(range_info))
    range_info
  },

  # Temporary scale for the purpose of calling break_info()
  create_scale = function(self, range, trans = identity_trans()) {
    scale <- ggproto(NULL, ScaleContinuousPosition,
                     name = self$name,
                     breaks = self$breaks,
                     labels = self$labels,
                     limits = range,
                     expand = c(0, 0),
                     trans = trans
    )
    scale$train(range)
    scale
  },
  make_title = function(title) {
    title
  }
)
