#' @include layer.R
#' @include scale-type.R
NULL

#' Produce boilerplate constructors
#'
#' The `boilerplate()` functions sets up a user-facing constructor for ggproto
#' classes. Currently, `boilerplate()` is implemented for `Geom` classes.
#'
#' @param x An object to setup a constructor for.
#' @param ... Name-value pairs to use as additional arguments in the
#'   constructor. For layers, these are passed on to [`layer(params)`][layer()].
#' @param checks A list of calls to be evaluated before construction of the
#'   object, such as one constructed with [`exprs()`][rlang::exprs()].
#' @param env An environment to search for the object.
#'
#' @return A function
#' @export
#' @keywords internal
#'
#' @examples
#' # For testing purposes, a geom that returns grobs
#' GeomTest <- ggproto(
#'   "GeomTest", Geom,
#'   draw_group = function(..., grob = grid::pointsGrob()) {
#'     return(grob)
#'   }
#' )
#' # Creating a constructor
#' geom_test <- boilerplate(GeomTest)
#'
#' # Note that `grob` is automatically an argument to the function
#' names(formals(geom_test))
#'
#' # Use in a plot
#' set.seed(1234)
#' p <- ggplot(mtcars, aes(disp, mpg))
#' p + geom_test()
#' p + geom_test(grob = grid::circleGrob())
boilerplate <- function(x, ...) {
  UseMethod("boilerplate")
}

#' @export
#' @rdname boilerplate
boilerplate.Geom <- function(x, ..., checks = NULL, env = caller_env()) {

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
  fmls <- pairlist2(
    mapping  = args$mapping,
    data     = args$data,
    stat     = args$stat %||% "identity",
    position = args$position %||% "identity",
    `...` = missing_arg(),
    !!!args[extra_args],
    na.rm    = args$na.rm %||% FALSE,
    show.legend = args$show.legend %||% NA,
    inherit.aes = args$inherit.aes %||% TRUE
  )

  # Construct call for the 'layer(params)' argument
  params <- exprs(!!!syms(c("na.rm", extra_args)), .named = TRUE)
  params <- call2("list2", !!!params, quote(...))

  # Construct rest of 'layer()' call
  layer_args <- syms(setdiff(fixed_fmls_names, c("...", "na.rm")))
  layer_args <- append(layer_args, list(geom = geom), after = 2)
  layer_args <- exprs(!!!layer_args, params = !!params, .named = TRUE)
  body <- call2("layer", !!!layer_args)

  # Prepend any checks
  if (!missing(checks)) {
    lang <- vapply(checks, is_call, logical(1))
    if (!all(lang)) {
      cli::cli_abort(
        "{.arg checks} must be a list of calls, such as one constructed \\
        with {.fn rlang::exprs}."
      )
    }
    body <- call2("{", !!!checks, body)
  }

  # We encapsulate rlang::list2
  new_env <- new_environment(list(list2 = list2), env)

  new_function(fmls, body, env = new_env)
}
