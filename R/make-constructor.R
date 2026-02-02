#' @include layer.R
#' @include scale-type.R
NULL

#' Produce boilerplate constructors
#'
#' The `make_constructor()` functions sets up a user-facing constructor for
#' ggproto classes. Currently, `make_constructor()` is implemented for
#' `Geom` classes.
#'
#' @param x An object to setup a constructor for.
#' @param ... Name-value pairs to use as additional arguments in the
#'   constructor. For layers, these are passed on to [`layer(params)`][layer()].
#' @param checks A list of calls to be evaluated before construction of the
#'   object, such as one constructed with [`exprs()`][rlang::exprs()].
#' @param omit A character vector of automatically retrieved argument names
#'   that should not be converted to user-facing arguments. Useful for
#'   internally computed variables.
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
#' geom_test <- make_constructor(GeomTest)
#'
#' # Note that `grob` is automatically an argument to the function
#' names(formals(geom_test))
#'
#' # Use in a plot
#' set.seed(1234)
#' p <- ggplot(mtcars, aes(disp, mpg))
#' p + geom_test()
#' p + geom_test(grob = grid::circleGrob())
#'
#' # The `checks` argument can be used to evaluate arbitrary expressions in
#' # the constructor before building the layer.
#' geom_path2 <- make_constructor(
#'   GeomPath, checks = rlang::exprs(
#'     match.arg(lineend, c("butt", "round", "square")),
#'     match.arg(linejoin, c("round", "mitre", "bevel"))
#'   )
#' )
#'
#' # Note the inclusion of the expressions
#' print(geom_path2)
#'
#' # Argument mismatch is detected
#' try(geom_path2(linejoin = "foo"))
make_constructor <- function(x, ...) {
  UseMethod("make_constructor")
}

#' @export
#' @rdname make_constructor
make_constructor.Geom <- function(x, ..., checks = exprs(), omit = character(),
                                  env = caller_env()) {

  # Check that we can independently find the geom
  geom <- gsub("^geom_", "", snake_class(x))
  validate_subclass(geom, "Geom", env = env)

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
    unique(c(names(args), fixed_fmls_names, "flipped_aes", x$aesthetics(), omit))
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
    extra_args <- intersect(extra_args, names(args))
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
  if (length(exprs) > 0) {
    lang <- vapply(checks, is_call, logical(1))
    if (!all(lang)) {
      cli::cli_abort(
        "{.arg checks} must be a list of calls, such as one constructed \\
        with {.fn rlang::exprs}."
      )
    }
  }
  body <- call2("{", !!!checks, body)

  # We encapsulate rlang::list2
  new_env <- new_environment(list(list2 = list2_wrapper), env)

  new_function(fmls, body, new_env)
}

#' @export
#' @rdname make_constructor
make_constructor.Stat <- function(x, ..., checks = exprs(), omit = character(),
                                  env = caller_env()) {
  # Check that we can independently find the stat
  stat <- gsub("^stat_", "", snake_class(x))
  validate_subclass(stat, "Stat", env = env)

  # Split additional arguments into required and extra ones
  args <- enexprs(...)
  fixed_fmls_names <- c("mapping", "data", "geom", "position", "...",
                        "na.rm", "show.legend", "inherit.aes")
  extra_args <- setdiff(names(args), fixed_fmls_names)
  if ("stat" %in% extra_args) {
    cli::cli_abort("{.arg stat} is a reversed argument.")
  }

  known_params <-
    unique(c(names(args), fixed_fmls_names, "flipped_aes", x$aesthetics(), omit))
  missing_params <- setdiff(x$parameters(), known_params)

  # Fill in missing parameters from the compute methods
  if (length(missing_params) > 0) {
    compute_args <- ggproto_formals(x$compute_panel)
    if ("..." %in% names(compute_args)) {
      compute_args <- ggproto_formals(x$compute_group)
    }
    params <- intersect(missing_params, names(compute_args))
    extra_args <- c(extra_args, params)
    for (param in params) {
      if (!identical(compute_args[[param]], missing_arg())) {
        args[param] <- compute_args[param]
      }
    }
    extra_args <- intersect(extra_args, names(args))
    missing_params <- setdiff(missing_params, names(args))
    if (length(missing_params) > 0) {
      cli::cli_warn(
        "In {.fn stat_{stat}}: please consider providing default values for: \\
        {missing_params}."
      )
    }
  }

  # Build function formals
  fmls <- pairlist2(
    mapping  = args$mapping,
    data     = args$data,
    geom     = args$geom %||% cli::cli_abort("{.arg geom} is required."),
    position = args$position %||% "identity",
    `...`    = missing_arg(),
    !!!args[extra_args],
    na.rm = args$na.rm %||% FALSE,
    show.legend = args$show.legend %||% NA,
    inherit.aes = args$inherit.aes %||% TRUE
  )

  # Construct params for the `layer(params)` argument
  params <- exprs(!!!syms(c("na.rm", extra_args)), .named = TRUE)
  params <- call2("list2", !!!params, quote(...))

  # Construct rest of `layer()` call
  layer_args <- syms(setdiff(fixed_fmls_names, c("...", "na.rm")))
  layer_args <- append(layer_args, list(stat = stat), after = 3)
  layer_args <- exprs(!!!layer_args, params = !!params, .named = TRUE)
  body <- call2("layer", !!!layer_args)

  # Prepend any checks
  if (length(exprs) > 0) {
    lang <- vapply(checks, is_call, logical(1))
    if (!all(lang)) {
      cli::cli_abort(
        "{.arg checks} must be a list of calls, such as one constructed \\
        with {.fn rlang::exprs}."
      )
    }
  }
  body <- call2("{", !!!checks, body)

  # We encapsulate rlang::list2
  new_env <- new_environment(list(list2 = list2_wrapper), env)

  new_function(fmls, body, new_env)
}

list2_wrapper = function(...) {
  rlang::list2(...)
}
