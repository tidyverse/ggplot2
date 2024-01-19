#' @include guide-axis.R
NULL

#' Axis with logarithmic tick marks
#'
#' This axis guide replaces the placement of ticks marks at intervals in
#' log10 space.
#'
#' @param long,mid,short A [grid::unit()] object or [rel()] object setting
#'   the (relative) length of the long, middle and short ticks. Numeric values
#'   are interpreted as [rel()] objects. The [rel()] values are used to multiply
#'   values of the `axis.ticks.length` theme setting.
#' @param prescale_base Base of logarithm used to transform data manually. The
#'   default, `NULL`, will use the scale transformation to calculate positions.
#'   Only set `prescale_base` if the data has already been log-transformed.
#'   When using a log-transform in the position scale or in `coord_trans()`,
#'   keep the default `NULL` argument.
#' @param negative_small When the scale limits include 0 or negative numbers,
#'   what should be the smallest absolute value that is marked with a tick?
#' @param short_theme A theme [element][element_line()] for customising the
#'   display of the shortest ticks. Must be a line or blank element, and
#'   it inherits from the `axis.minor.ticks` setting for the relevant position.
#' @param expanded Whether the ticks should cover the range after scale
#'   expansion (`TRUE`, default), or be restricted to the scale limits
#'   (`FALSE`).
#' @inheritParams guide_axis
#' @inheritDotParams guide_axis -minor.ticks
#'
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(msleep, aes(bodywt, brainwt)) +
#'   geom_point(na.rm = TRUE)
#'
#' # The logticks axis works well with log scales
#' p + scale_x_log10(guide = "axis_logticks") +
#'   scale_y_log10(guide = "axis_logticks")
#'
#' # Or with log-transformed coordinates
#' p + coord_trans(x = "log10", y = "log10") +
#'   guides(x = "axis_logticks", y = "axis_logticks")
#'
#' # When data is transformed manually, one should provide `prescale_base`
#' # Keep in mind that this axis uses log10 space for placement, not log2
#' p + aes(x = log2(bodywt), y = log10(brainwt)) +
#'   guides(
#'     x = guide_axis_logticks(prescale_base = 2),
#'     y = guide_axis_logticks(prescale_base = 10)
#'   )
#'
#' # A plot with both positive and negative extremes, pseudo-log transformed
#' set.seed(42)
#' p2 <- ggplot(data.frame(x = rcauchy(1000)), aes(x = x)) +
#'   geom_density() +
#'   scale_x_continuous(
#'     breaks = c(-10^(4:0), 0, 10^(0:4)),
#'     transform = "pseudo_log"
#'   )
#'
#' # The log ticks are mirrored when 0 is included
#' p2 + guides(x = "axis_logticks")
#'
#' # To control the tick density around 0, one can set `negative_small`
#' p2 + guides(x = guide_axis_logticks(negative_small = 1))
guide_axis_logticks <- function(
  long  = 2.25,
  mid   = 1.5,
  short = 0.75,
  prescale_base = NULL,
  negative_small = 0.1,
  short_theme = element_line(),
  expanded = TRUE,
  cap = "none",
  theme = NULL,
  ...
) {
  if (is.logical(cap)) {
    check_bool(cap)
    cap <- if (cap) "both" else "none"
  }
  cap <- arg_match0(cap, c("none", "both", "upper", "lower"))

  if (is_bare_numeric(long))   long <- rel(long)
  if (is_bare_numeric(mid))    mid  <- rel(mid)
  if (is_bare_numeric(short)) short <- rel(short)

  check_fun <- function(x) (is.rel(x) || is.unit(x)) && length(x) == 1
  what <- "a {.cls rel} or {.cls unit} object of length 1"
  check_object(long,  check_fun, what)
  check_object(mid,   check_fun, what)
  check_object(short, check_fun, what)
  check_number_decimal(
    negative_small, min = 1e-100, # minimal domain of scales::log_trans
    allow_infinite = FALSE,
    allow_null = TRUE
  )
  check_bool(expanded)
  check_inherits(short_theme, c("element_blank", "element_line"))

  new_guide(
    available_aes  = c("x", "y"),
    prescale_base  = prescale_base,
    negative_small = negative_small,
    expanded       = expanded,
    long  = long,
    mid   = mid,
    short = short,
    cap   = cap,
    minor.ticks = TRUE,
    short_theme = short_theme,
    theme = theme,
    ...,
    super = GuideAxisLogticks
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideAxisLogticks <- ggproto(
  "GuideAxisLogticks", GuideAxis,

  params = defaults(
    list(
      prescale_base  = NULL,
      negative_small = 0.1,
      minor.ticks    = TRUE, # for spacing calculation
      long  = 2.25,
      mid   = 1.5,
      short = 0.75,
      expanded = TRUE,
      short_theme = NULL
    ),
    GuideAxis$params
  ),

  # Here we calculate a 'shadow key' that only applies to the tickmarks.
  extract_params = function(scale, params, ...) {

    if (scale$is_discrete()) {
      cli::cli_abort("Cannot calculate logarithmic ticks for discrete scales.")
    }

    aesthetic   <- params$aesthetic
    params$name <- paste0(params$name, "_", aesthetic)
    params

    # Reconstruct a transformation if user has prescaled data
    if (!is.null(params$prescale_base)) {
      trans_name <- scale$get_transformation()$name
      if (trans_name != "identity") {
        cli::cli_warn(paste0(
          "The {.arg prescale_base} argument will override the scale's ",
          "{.field {trans_name}} transformation in log-tick positioning."
        ))
      }
      transformation <- transform_log(base = params$prescale_base)
    } else {
      transformation <- scale$get_transformation()
    }

    # Reconstruct original range
    limits <- transformation$inverse(scale$get_limits())
    has_negatives <- any(limits <= 0)

    if (!has_negatives) {
      start <- floor(log10(min(limits))) - 1L
      end   <- ceiling(log10(max(limits))) + 1L
    } else {
      params$negative_small <- params$negative_small %||% 0.1
      start <- floor(log10(abs(params$negative_small)))
      end   <- ceiling(log10(max(abs(limits)))) + 1L
    }

    # Calculate tick marks
    tens  <- 10^seq(start, end, by = 1)
    fives <- tens * 5
    ones  <- as.vector(outer(setdiff(2:9, 5), tens))

    if (has_negatives) {
      # Filter and mirror ticks around 0
      tens  <- tens[tens >= params$negative_small]
      tens  <- c(tens, -tens, 0)
      fives <- fives[fives >= params$negative_small]
      fives <- c(fives, -fives)
      ones  <- ones[ones >= params$negative_small]
      ones  <- c(ones, -ones)
    }

    # Set ticks back into transformed space
    ticks  <- transformation$transform(c(tens, fives, ones))
    nticks <- c(length(tens), length(fives), length(ones))

    logkey <- data_frame0(
      !!aesthetic := ticks,
      .type = rep(1:3, times = nticks)
    )

    # Discard out-of-bounds ticks
    range <- if (params$expanded) scale$continuous_range else scale$get_limits()
    logkey <- vec_slice(logkey, ticks >= range[1] & ticks <= range[2])

    # Adjust capping based on these ticks instead of regular ticks
    if (params$cap %in% c("both", "upper")) {
      params$decor[[aesthetic]][2] <- max(logkey[[aesthetic]])
    }
    if (params$cap %in% c("both", "lower")) {
      params$decor[[aesthetic]][1] <- min(logkey[[aesthetic]])
    }

    params$logkey <- logkey
    params
  },

  transform = function(self, params, coord, panel_params) {
    params <- GuideAxis$transform(params, coord, panel_params)
    # Also transform the logkey
    params$logkey <- coord$transform(params$logkey, panel_params)
    params
  },

  override_elements = function(params, elements, theme) {
    elements <- GuideAxis$override_elements(params, elements, theme)
    length <- elements$major_length

    # Inherit short ticks from minor ticks
    elements$short <- combine_elements(params$short_theme, elements$minor)

    # Multiply rel units with theme's tick length
    tick_length <- lapply(params[c("long", "mid", "short")], function(x) {
      if (is.unit(x)) x else unclass(x) * length
    })
    tick_length <- inject(unit.c(!!!tick_length))
    elements$tick_length  <- tick_length

    # We replace the lengths so that spacing calculation works out as intended
    elements$major_length <- max(tick_length)
    elements$minor_length <- min(tick_length)
    elements
  },

  build_ticks = function(key, elements, params, position = params$opposite) {
    # Instead of passing regular key, we pass the logkey
    key <- params$logkey
    long <- Guide$build_ticks(
      vec_slice(key, key$.type == 1L),
      elements$ticks, params, position,
      elements$tick_length[1L]
    )

    mid <- Guide$build_ticks(
      vec_slice(key, key$.type == 2L),
      elements$minor, params, position,
      elements$tick_length[2L]
    )

    short <- Guide$build_ticks(
      vec_slice(key, key$.type == 3L),
      elements$short, params, position,
      elements$tick_length[3L]
    )
    grobTree(long, mid, short, name = "ticks")
  }
)
