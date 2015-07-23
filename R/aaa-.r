#' @include proto2.r
NULL

#' Base proto2 classes for ggplot2
#'
#' If you are creating a new geom, stat or positions in another package, you'll
#' need to extend from \code{ggplot2::Geom}, \code{ggplot2::Stat} or
#' \code{ggplot2::Position}.
#'
#' @export Geom Stat Position
#' @aliases Geom Stat Position
#' @keywords internal
#' @name ggplot2-proto2
NULL

TopLevel <- proto2("TopLevel", NULL,
  find = function(self, name) {
    # Convert name to camel case
    name <- camelize(name, first = TRUE)
    fullname <- paste0(firstUpper(self$type), name)

    if (!exists(fullname)) {
      stop("No ", self$type, " called ", fullname, call. = FALSE)
    }
    get(fullname)
  },

  # Convert class name from camel case (GeomBar) to snake case (geom_bar).
  my_name = function(self) {
    snakeize(class(self)[1])
  },

  type = "toplevel"
)
