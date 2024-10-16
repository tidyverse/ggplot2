#' @include layer.R
#' @include scale-type.R
NULL

#' @export
boilerplate <- function(x, ...) {
  UseMethod("boilerplate")
}

#' @export
boilerplate.Geom <- function(x, ..., env = caller_env()) {

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
  body <- as.call(parse(text = body))[[1]]

  new_function(fmls, body)
}
