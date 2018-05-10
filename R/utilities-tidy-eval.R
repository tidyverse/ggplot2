#' Tidy eval helpers
#'
#' @description
#'
#' * \code{\link[rlang]{sym}()} creates a symbol from a string and
#'   \code{\link[rlang]{syms}()} creates a list of symbols from a
#'   character vector.
#'
#' * \code{\link[rlang]{expr}()} and \code{\link[rlang]{quo}()} quote
#'   one expression. `quo()` wraps the quoted expression in a quosure.
#'
#'   The plural variants \code{\link[rlang]{exprs}()} and
#'   \code{\link[rlang]{quos}()} return a list of quoted expressions or
#'   quosures.
#'
#' * \code{\link[rlang]{enexpr}()} and \code{\link[rlang]{enquo}()}
#'   capture the expression supplied as argument by the user of the
#'   current function (`enquo()` wraps this expression in a quosure).
#'
#'   \code{\link[rlang]{enexprs}()} and \code{\link[rlang]{enquos}()}
#'   capture multiple expressions supplied as arguments, including
#'   `...`.
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @aliases          quo quos enquo enquos quo_name
#'                   sym ensym syms ensyms
#'                   expr exprs enexpr enexprs
#'                   .data
#' @export           quo quos enquo enquos quo_name
#' @export           sym ensym syms ensyms
#' @export           expr enexpr enexprs
#' @export           .data
NULL

#' @importFrom rlang quo quos enquo enquos quo_name sym ensym syms
#'   ensyms expr exprs enexpr enexprs .data
NULL
