# Print accessors
# Write out all convenience accessor functions to R file.
#
# @keyword internal
accessors_print <- function(file = "") {
  funs <- sort(c(
    Geom$accessors(), Stat$accessors(), Scale$accessors(),
    Coord$accessors(),  Position$accessors(), Facet$accessors()
  ))
  cat(funs, file=file, sep="")
}

TopLevel$accessors <- function(.) {
  accessors <- lapply(.$find_all(), function(y) y$create_accessor())
  unname(unlist(accessors))
}
 
TopLevel$create_accessor <- function(.) {
  paste(.$my_name(), " <- ", .$myName(), "$build_accessor()\n", sep="")
}
Scale$create_accessor <- function(.) {
  if (is.null(.$common)) {
    var <- NULL
    short <- paste(.$class(), .$objname, sep="_")
  } else {
    var <- paste("list(variable = \"\\\"", .$common, "\\\"\")", sep="")
    short <- paste(.$class(), .$common, .$objname, sep="_")
  }

  paste(short, " <- ", .$myName(), "$build_accessor(", var, ")\n", sep="")
}


TopLevel$build_accessor <- function(., extra_args = c()) {
  layer <- if (.$class() %in% c("geom","stat")) c(
    list(mapping=NULL,data=NULL),
    compact(list(
      geom = if (exists("default_geom", .)) .$default_geom()$objname, 
      stat = if (exists("default_stat", .)) .$default_stat()$objname, 
      position = if (exists("default_pos", .)) .$default_pos()$objname
    ))
  )
  params <- .$params()
  params <- params[names(params) != "..."]
  if (.$class() %in% c("geom","stat")) params <- params[!sapply(params, is.null)]
  args <- c(layer, params)
  
  body <- ps(
    .$myName(), "$", "new(",
    if (length(args) > 0) ps(names(args),"=", names(args), collase =", "), 
    if (length(extra_args) > 0) ps(names(extra_args),"=", extra_args, collase =", "), 
    "...",
    ")"
  )
  f <- function() {}
  formals(f) <- as.pairlist(c(args, alist(... =)))
  body(f) <- parse(text = body)
  environment(f) <- globalenv()
  f
}
