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

# INCLUDES <- "web/graphics"
# FILETYPE <- "html"

# Upper case first letter of string
# This comes from the examples of some R function.
#
# @keyword internal
firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="")
}


TopLevel <- proto2(
  members = list(
    find = function(self, name) {
      # Convert name to camel case
      name <- gsub("_(.)", "\\U\\1", name, perl = TRUE)
      fullname <- paste0(firstUpper(self$class()), firstUpper(name))

      if (!exists(fullname)) {
        stop("No ", self$class(), " called ", fullname, call.=FALSE)
      }
      get(fullname)
    },

    # Convert class name from camel case (GeomBar) to snake case (geom_bar).
    my_name = function(self) {
      name <- class(self)[1]
      name <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", name)
      name <- gsub(".", "_", name, fixed = TRUE)
      name <- gsub("([a-z])([A-Z])", "\\1_\\2", name)
      name <- tolower(name)
      name
    },

    params = function(self) {
      param <- self$parameters()
      if (length(param) == 0) return()

      if(!exists("required_aes", .)) return(param)

      aesthetics <- c(self$required_aes, names(self$default_aes()))
      param[setdiff(names(param), aesthetics)]
    },

    class = function(self) "toplevel"
  )
)


