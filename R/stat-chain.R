stat_chain <- function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    ...,
    stats = "identity",
    stat.params = list(),
    redirect = list(),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatChain,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      stats = stats,
      stat.params = stat.params,
      redirect = redirect,
      ...
    )
  )
}

StatChain <- ggproto(
  "StatChain", Stat,

  extra_params = c("na.rm", "stats", "stat.params", "redirect"),

  setup_params = function(data, params) {
    params$stats <- lapply(params$stats, validate_subclass, subclass = "Stat")
    n_stats <- length(params$stats)

    params$stat.params <- force_length(
      params$stat.params, n_stats,
      warn_longer = TRUE, arg = "stat.params"
    )

    params$redirect <- force_length(
      params$redirect, n_stats,
      warn_longer = TRUE, arg = "redirect"
    )

    params
  },

  compute_layer = function(self, data, params, layout) {

    n_stats <- length(params$stats)

    for (i in seq_len(n_stats)) {
      stat <- params$stats[[i]]
      param <- params$stat.params[[i]]

      # We repeat the `layer()` duty of rejecting unknown parameters
      valid <- stat$parameters(TRUE)
      extra_param <- setdiff(names(param), valid)
      if (length(extra_param) > 0) {
        cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}.")
      }
      param <- param[intersect(names(param), valid)]
      if (length(param) < 1) {
        param <- list()
      }

      # Repeat `Layer$compute_statistic()` duty
      computed_param <- stat$setup_params(data, param)
      computed_param$na.rm <- computed_param$na.rm %||% params$na.rm
      data <- stat$setup_data(data, computed_param)
      data <- stat$compute_layer(data, computed_param, layout)
      if (nrow(data) < 1) {
        return(data)
      }

      # Repeat `Layer$map_statistic()` duty, skipping backtransforms and such
      aes <- stat$default_aes[is_calculated_aes(stat$default_aes)]
      aes <- aes[setdiff(names(aes), names(data))]
      aes <- compact(defaults(params$redirect[[i]], aes))
      if (length(aes) == 0) {
        next
      }
      new <- eval_aesthetics(substitute_aes(aes), data)
      check_nondata_cols(
        new, aes,
        problem = "Aesthetics must be valid computed stats.",
        hint    = "Did you specify the `redirect` argument correctly?"
      )
      data[names(new)] <- new
    }

    data
  }
)

force_length <- function(x, n = length(x), padding = list(NULL),
                         warn_longer = FALSE, warn_shorter = FALSE,
                         arg = caller_arg(x)) {
  force(arg)
  nx <- length(x)
  if (nx == n) {
    return(x)
  }
  n_pad <- n - nx
  if (n_pad > 0) {
    x <- c(x, rep(padding, length = n_pad))
    if (isTRUE(warn_shorter)) {
      cli::cli_warn(
        "Padded {.arg {arg}} with {n_pad} element{?s}."
      )
    }
  } else if (n_pad < 0) {
    x <- x[seq_len(n)]
    if (isTRUE(warn_longer)) {
      cli::cli_warn(
        "Dropped {abs(n_pad)} excess element{?s} from {.arg {arg}}."
      )
    }
  }
  x
}
