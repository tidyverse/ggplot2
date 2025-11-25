#' Set ggplot2 edition
#'
#' ggplot2 uses the 'edition' concept to manage the lifecycles of functions and
#' arguments. Setting a recent edition opens up the latest features but also
#' closes down deprecated and superseded functionality.
#'
#' @param edition An edition. Possible values currently include `"2026"` only.
#'   Can be `NULL` (default) to unset an edition.
#'
#' @returns The previous `edition` value. This function is called for the side
#'  effect of setting the edition though.
#' @export
#'
#' @examples
#' set_edition(2026)
set_edition <- function(edition = NULL) {
  old <- ggplot_global$edition
  ggplot_global$edition <- validate_edition(edition)
  invisible(old)
}

get_edition <- function() {
  ggplot_global$edition[[1]]
}

# Any new editions should be appended here and anchored to a version
edition_versions <- c(
  "2025" = "4.0.0",
  "2026" = "4.1.0"
)

validate_edition <- function(edition, allow_null = TRUE, call = caller_env()) {
  if (is.null(edition) && allow_null) {
    return(NULL)
  }
  edition <- as.character(edition)
  check_string(edition, allow_empty = FALSE, call = call)
  arg_match0(edition, names(edition_versions), error_call = call)
}

edition_require <- function(edition = NULL, what, call = caller_env()) {
  edition <- validate_edition(edition)
  current_edition <- get_edition()
  if (as.numeric(current_edition) >= as.numeric(edition)) {
    return(invisible())
  }
  cli::cli_abort(
    "{what} requires the {edition} edition of {.pkg ggplot2}.",
    call = call
  )
}

deprecate <- function(when, ..., id = NULL, always = FALSE, user_env = NULL,
                      escalate = NULL) {

  defunct <- "3.0.0"
  full    <- "3.4.0"
  soft    <- utils::packageVersion("ggplot2")

  if (identical(escalate, "delay")) {
    soft <- full
    full <- defunct
    defunct <- "0.0.0"
  }

  edition <- get_edition()
  if (!is.null(edition) && edition %in% names(edition_versions)) {
    soft <- full <- defunct <- edition_versions[[edition]]
  }

  version <- as.package_version(when)
  if (version < defunct || identical(escalate, "abort")) {
    lifecycle::deprecate_stop(when, ...)
  }
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% caller_env(2)
  if (version <= full || identical(escalate, "warn")) {
    lifecycle::deprecate_warn(when, ..., id = id, always = always, user_env = user_env)
  } else if (version <= soft) {
    lifecycle::deprecate_soft(when, ..., id = id, user_env = user_env)
  }
  invisible()
}

supersede <- function(edition, what, with = NULL, ..., env = caller_env()) {
  current_edition <- get_edition()
  if (
    !is.null(current_edition) &&
    current_edition %in% names(edition_versions) &&
    as.numeric(current_edition) >= as.numeric(edition)
  ) {
    lifecycle::deprecate_stop(
      when = paste0("edition ", edition),
      what = what,
      with = with,
      env  = env
    )
  }
  lifecycle::signal_stage(
    stage = "superseded",
    what  = what,
    with  = with,
    env   = env
  )
}
