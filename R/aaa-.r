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
    find_all = function(only.documented = FALSE) {
      names <- ls(
        pattern = paste("^", firstUpper(self$class()), "[A-Z].+", sep=""),
        parent.env(TopLevel)
      )
      objs <- structure(lapply(names, get), names=names)

      if (only.documented) objs <- objs[sapply(objs, function(x) get("doc", x))]
      objs
    },
    find = function(name) {
      fullname <- paste0(firstUpper(self$class()), firstUpper(name))

      if (!exists(fullname)) {
        stop("No ", self$class(), " called ", name, call.=FALSE)
      }
      get(fullname)
    },

    my_name = function(prefix=TRUE) {
      if (!prefix) return(self$objname)
      paste(self$class(), self$objname, sep="_")
    },
    my_names = function() self$my_name(),

    myName = function() {
      ps(firstUpper(self$class()), ps(firstUpper(strsplit(self$objname, "_")[[1]])))
    },

    params = function() {
      param <- self$parameters()
      if (length(param) == 0) return()

      if(!exists("required_aes", .)) return(param)

      aesthetics <- c(self$required_aes, names(self$default_aes()))
      param[setdiff(names(param), aesthetics)]
    },

    class = function() "toplevel"
  )
)


