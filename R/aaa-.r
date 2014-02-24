# INCLUDES <- "web/graphics"
# FILETYPE <- "html"

# Upper case first letter of string
# This comes from the examples of some R function.
#
# @keyword internal
firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="")
}

TopLevel <- proto(expr = {
  find_all <- function(., only.documented = FALSE) {
    names <- ls(pattern=paste("^", firstUpper(.$class()), "[A-Z].+", sep=""), parent.env(TopLevel))
    objs <- structure(lapply(names, get), names=names)

    if (only.documented) objs <- objs[sapply(objs, function(x) get("doc", x))]
    objs
  }
  find <- function(., name) {
    fullname <- paste(firstUpper(.$class()), firstUpper(name), sep="")
    if (!exists(fullname)) {
      stop("No ", .$class(), " called ", name, call.=FALSE)
    }
    get(fullname)
  }

  my_name <- function(., prefix=TRUE) {
    if (!prefix) return(.$objname)
    paste(.$class(), .$objname, sep="_")
  }
  my_names <- function(.) .$my_name()

  myName <- function(.) {
    ps(firstUpper(.$class()), ps(firstUpper(strsplit(.$objname, "_")[[1]])))
  }

  params <- function(.) {
    param <- .$parameters()
    if (length(param) == 0) return()

    if(!exists("required_aes", .)) return(param)

    aesthetics <- c(.$required_aes, names(.$default_aes()))
    param[setdiff(names(param), aesthetics)]
  }

})

#' @export
print.proto <- function(x, ...) x$pprint(...)
pprint <- function(x, ...) print(as.list(x), ...)
# name.proto <- function (...) {
#        proto(print.proto = print.default, f = proto::name.proto)$f(...)
# }


