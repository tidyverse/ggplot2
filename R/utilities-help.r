## Information about aesthetics used by a geom or stat
##
## @param x A proto object inheriting from either Geom or Stat
## @return a \code{data.frame} with columns
## \describe{
## \item{aesthetic}{The name of the aesthetic, as a charater string}
## \item{required}{Is the aesthetic requried, as a logical}
## \item{default}{A list of the default values expressed as character}
## }
## The \code{data.frame} is sorted with the requried aesthetics first
## and alphabetically by aesthetic name within that.
aesthetics <- function(x) {
    req_aes <- x$required_aes
    def_aes <- x$default_aes()
    allowed_aes <- data.frame(aesthetic = unique(as.character(c(req_aes, names(def_aes)))),
                              stringsAsFactors = FALSE)
    allowed_aes$required <- rep(FALSE, nrow(allowed_aes))
    allowed_aes$required[allowed_aes$aesthetic %in% req_aes] <- TRUE
    calls <- vapply(def_aes, is.call, logical(1))
    def_aes[calls] <- lapply(def_aes[calls], eval)
    allowed_aes$default <- lapply(def_aes[allowed_aes$aesthetic], as.character)
    allowed_aes[order(!allowed_aes$required, allowed_aes$aesthetic),]
}

rd_aesthetics <- function(type, name) {
    obj <- get(firstUpper(type))
    aes <- aesthetics(obj$find(name))

    paste("\\code{", type, "_", name, "} ",
          "understands the following aesthetics (required aesthetics are in bold):\n\n",
          "\\itemize{\n",
          paste("  \\item \\code{",
                ifelse(aes$required,
                       paste("\\strong{", aes$aesthetic, "}", sep=""),
                       aes$aesthetic),
                "}",
                ifelse(sapply(aes$default, identical, character(0)),
                       "",
                       paste(" (default value: ", aes$default, ")", sep="")),
                collapse = "\n", sep = ""),
          "\n}\n", sep = "")
}

geom_with_aesthetic <- function(a) {
    geoms <- Geom$find_all()
    aesthetics <- llply(geoms, aesthetics)
    geoms[laply(aesthetics, function(aa) {any(aa$aesthetic == a)})]
}

rd_geom_with_aesthetic <- function(a) {
    geoms <- geom_with_aesthetic(a)
    geoms <- vapply(geoms, function(g) {g$my_name()}, character(1))
    paste("The aesthetic \\code{",
          a,
          "} is understood by the following geoms:\n\n",
          "\\itemize{\n",
          paste("  \\item\\code{\\link{",
                geoms,
                "}}",
                sep = "", collapse = "\n"),
          "\n}\n",
          sep = "")
}
