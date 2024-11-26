
ggplot_edition <- new.env(parent = emptyenv())

local_edition <- function(edition, .env = parent.frame()) {
  stopifnot(is_zap(edition) || (is.numeric(edition) && length(edition) == 1))
  pkg <- get_pkg_name(.env)
  local_bindings(!!pkg := edition, .env = ggplot_edition, .frame = .env)
}

edition_get <- function(.env = parent.frame(), default = 2024L) {
  pkg <- get_pkg_name(.env)

  # Try to query edition from cache
  edition <- env_get(ggplot_edition, nm = pkg, default = NULL)
  if (!is.null(edition)) {
    return(edition)
  }

  # Try to query package description
  desc <- find_description(path = ".", package = pkg)
  if (is.null(desc)) {
    return(default)
  }

  # Look up edition from the description
  field_name <- "Config/ggplot2/edition"
  edition <- as.integer(desc$get_field(field_name, default = default))

  # Cache result
  env_bind(ggplot_edition, !!pkg := edition)
  return(edition)
}

edition_deprecate <- function(edition, ..., .env = parent.frame()) {
  check_number_whole(edition)
  if (edition_get(.env) < edition) {
    return(invisible(NULL))
  }

  edition <- I(paste0("edition ", edition))
  lifecycle::deprecate_stop(edition, ...)
}

edition_require <- function(edition, what, .env = parent.frame()) {
  check_number_whole(edition)
  current <- edition_get(.env)
  if (current >= edition) {
    return(invisible(NULL))
  }
  msg <- paste0(what, " requires the ", edition, " edition of {.pkg ggplot2}.")
  cli::cli_abort(msg)
}

find_description <- function(path, package = NULL) {
  if (!is.null(package)) {
    return(desc::desc(package = package))
  } else {
    try_fetch(
      pkgload::pkg_desc(path),
      error = function(e) NULL
    )
  }
}

get_pkg_name <- function(env = parent.frame()) {
  env  <- topenv(env)
  name <- environmentName(env)
  if (!isNamespace(env) && name != "R_GlobalEnv") {
    return(NULL)
  }
  name
}
