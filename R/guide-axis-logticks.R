
guide_axis_logticks <- function(
  long  = 2.25,
  mid   = 1.5,
  short = 0.75,
  prescale_base = NULL,
  negative_small = 0.1,
  expanded = TRUE,
  cap = "none",
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
  check_bool(expanded)

  new_guide(
    available_aes  = c("x", "y"),
    prescale_base  = prescale_base,
    negative_small = negative_small,
    expanded       = expanded,
    long  = long,
    mid   = mid,
    short = short,
    minor.ticks = TRUE,
    ...,
    super = GuideAxisLogticks
  )
}


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
      expanded = TRUE
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
      trans_name <- scale$scale$trans$name
      if (trans_name != "identity") {
        cli::cli_warn(paste0(
          "The {.arg prescale_base} argument will override the scale's ",
          "{.field {trans_name}} transformation in log-tick positioning."
        ))
      }
      trans <- log_trans(base = params$prescale_base)
    } else {
      trans <- scale$scale$trans
    }

    # Reconstruct original range
    limits <- trans$inverse(scale$get_limits())
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
    ticks  <- trans$transform(c(tens, fives, ones))
    nticks <- c(length(tens), length(fives), length(ones))

    logkey <- data_frame0(
      !!aesthetic := ticks,
      .type = rep(1:3, times = nticks)
    )

    # Discard out-of-bounds ticks
    range <- if (params$expanded) scale$continuous_range else scale$get_limits()
    logkey <- vec_slice(logkey, ticks >= range[1] & ticks <= range[2])

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
    # In addition, we pass tick lengths directly
    Guide$build_ticks(
      params$logkey,
      elements$ticks, params, position,
      elements$tick_length[params$logkey$.type]
    )
  }
)
