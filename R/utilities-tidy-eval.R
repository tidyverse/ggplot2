#' Tidy eval helpers
#'
#' @description
#' This page lists the tidy eval tools reexported in this package from
#' rlang. To learn about using tidy eval in scripts and packages at a
#' high level, see the [dplyr programming
#' vignette](https://dplyr.tidyverse.org/articles/programming.html)
#' and the [ggplot2 in packages
#' vignette](https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html).
#' The [Metaprogramming
#' section](https://adv-r.hadley.nz/metaprogramming.html) of [Advanced
#' R](https://adv-r.hadley.nz) may also be useful for a deeper dive.
#'
#' * The tidy eval operators `{{`, `!!`, and `!!!` are syntactic
#'   constructs which are specially interpreted by tidy eval functions.
#'   You will mostly need `{{`, as `!!` and `!!!` are more advanced
#'   operators which you should not have to use in simple cases.
#'
#'   The curly-curly operator `{{` allows you to tunnel data-variables
#'   passed from function arguments inside other tidy eval functions.
#'   `{{` is designed for individual arguments. To pass multiple
#'   arguments contained in dots, use `...` in the normal way.
#'
#'   ```
#'   my_function <- function(data, var, ...) {
#'     data %>%
#'       group_by(...) %>%
#'       summarise(mean = mean({{ var }}))
#'   }
#'   ```
#'
#' * [enquo()] and [enquos()] delay the execution of one or several
#'   function arguments. The former returns a single expression, the
#'   latter returns a list of expressions. Once defused, expressions
#'   will no longer evaluate on their own. They must be injected back
#'   into an evaluation context with `!!` (for a single expression) and
#'   `!!!` (for a list of expressions).
#'
#'   ```
#'   my_function <- function(data, var, ...) {
#'     # Defuse
#'     var <- enquo(var)
#'     dots <- enquos(...)
#'
#'     # Inject
#'     data %>%
#'       group_by(!!!dots) %>%
#'       summarise(mean = mean(!!var))
#'   }
#'   ```
#'
#'   In this simple case, the code is equivalent to the usage of `{{`
#'   and `...` above. Defusing with `enquo()` or `enquos()` is only
#'   needed in more complex cases, for instance if you need to inspect
#'   or modify the expressions in some way.
#'
#' * The `.data` pronoun is an object that represents the current
#'   slice of data. If you have a variable name in a string, use the
#'   `.data` pronoun to subset that variable with `[[`.
#'
#'   ```
#'   my_var <- "disp"
#'   mtcars %>% summarise(mean = mean(.data[[my_var]]))
#'   ```
#'
#' * Another tidy eval operator is `:=`. It makes it possible to use
#'   glue and curly-curly syntax on the LHS of `=`. For technical
#'   reasons, the R language doesn't support complex expressions on
#'   the left of `=`, so we use `:=` as a workaround.
#'
#'   ```
#'   my_function <- function(data, var, suffix = "foo") {
#'     # Use `{{` to tunnel function arguments and the usual glue
#'     # operator `{` to interpolate plain strings.
#'     data %>%
#'       summarise("{{ var }}_mean_{suffix}" := mean({{ var }}))
#'   }
#'   ```
#'
#' * Many tidy eval functions like `dplyr::mutate()` or
#'   `dplyr::summarise()` give an automatic name to unnamed inputs. If
#'   you need to create the same sort of automatic names by yourself,
#'   use `as_label()`. For instance, the glue-tunnelling syntax above
#'   can be reproduced manually with:
#'
#'   ```
#'   my_function <- function(data, var, suffix = "foo") {
#'     var <- enquo(var)
#'     prefix <- as_label(var)
#'     data %>%
#'       summarise("{prefix}_mean_{suffix}" := mean(!!var))
#'   }
#'   ```
#'
#'   Expressions defused with `enquo()` (or tunnelled with `{{`) need
#'   not be simple column names, they can be arbitrarily complex.
#'   `as_label()` handles those cases gracefully. If your code assumes
#'   a simple column name, use `as_name()` instead. This is safer
#'   because it throws an error if the input is not a name as expected.
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @aliases expr enquo enquos sym syms .data as_label
#' @export expr enquo enquos sym syms .data as_label
NULL

# For backward-compatibility, keep exporting the old ones

#' @name tidyeval
#' @keywords internal
#' @aliases quo_name quo quos enexpr enexprs ensym ensyms
#' @export quo_name quo quos enexpr enexprs ensym ensyms
NULL
