#' @include layer.R
#' @include scale-type.R
NULL

#' @export
boilerplate <- function(x, ...) {
  UseMethod("boilerplate")
}

#' @export
boilerplate.Geom <- function(x, ..., checks, env = caller_env()) {

  # Check that we can independently find the geom
  geom <- gsub("^geom_", "", snake_class(x))
  check_subclass(geom, "Geom", env = env)

  # Split additional arguments into required and extra ones
  args <- enexprs(...)
  fixed_fmls_names <- c("mapping", "data", "stat", "position", "...",
                        "na.rm", "show.legend", "inherit.aes")
  extra_args <- setdiff(names(args), fixed_fmls_names)
  if ("geom" %in% extra_args) {
    cli::cli_abort("{.arg geom} is a reserved argument.")
  }

  # Fill in values for parameters from draw functions
  known_params <-
    unique(c(names(args), fixed_fmls_names, "flipped_aes", x$aesthetics()))
  missing_params <- setdiff(x$parameters(), known_params)
  if (length(missing_params) > 0) {
    draw_args <- ggproto_formals(x$draw_panel)
    if ("..." %in% names(draw_args)) {
      draw_args <- ggproto_formals(x$draw_group)
    }
    params <- intersect(missing_params, names(draw_args))
    extra_args <- c(extra_args, params)
    for (param in params) {
      if (!identical(draw_args[[param]], quote(expr = ))) {
        args[param] <- draw_args[param]
      }
    }
    missing_params <- setdiff(missing_params, names(args))
    if (length(missing_params) > 0) {
      cli::cli_warn(
        "In {.fn geom_{geom}}: please consider providing default values for: \\
        {missing_params}."
      )
    }
  }

  # Build function formals
  fmls <- list2(
    mapping  = args$mapping,
    data     = args$data,
    stat     = args$stat %||% "identity",
    position = args$position %||% "identity",
    `...` = quote(expr = ),
    !!!args[extra_args],
    na.rm    = args$na.rm %||% FALSE,
    show.legend = args$show.legend %||% NA,
    inherit.aes = args$inherit.aes %||% TRUE
  )

  if (length(extra_args) > 0) {
    extra_args <- paste0(
      "\n      ", extra_args, " = ", extra_args, ",", collapse = ""
    )
  }

  body <- paste0("
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = \"", geom, "\",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,",
      extra_args, "
      ...
    )
  )
  ")
  body <- str2lang(body)

  checks <- substitute(checks)
  if (!missing(checks)) {
    if (is_call(checks, "{")) {
      checks[[1]] <- NULL
    }
    body <- inject(quote(`{`(!!!c(checks, body))))
  }

  new_function(fmls, body)
}
