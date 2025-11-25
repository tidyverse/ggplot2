# Any new editions should be appended here and anchored to a version
edition_versions <- c(
  "2026" = "4.1.0"
)

get_edition <- function() {
  ggplot_global$edition[[1]]
}

set_edition <- function(edition = NULL) {
  ggplot_global$edition <- validate_edition(edition)
  invisible()
}

validate_edition <- function(edition, allow_null = TRUE, call = caller_env()) {
  if (is.null(edition) && allow_null) {
    return(NULL)
  }
  check_string(edition, allow_empty = FALSE, call = call)
  arg_match0(edition, names(edition_versions), error_call = call)
}

edition_require <- function(edition = NULL, what, call = caller_env()) {
  if (identical(get_edition(), validate_edition(edition))) {
    return(invisible())
  }
  cli::cli_abort(
    "{what} requires the {edition} edition of {.pkg ggplot2}.",
    call = call
  )
}
