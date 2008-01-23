TopLevel$examples <- function(.) {
  # Coming soon
}

TopLevel$examples_text <- function(.) {
  source <- attr(get("examples", .), "source")
  source <- source[-c(1, length(source))]
  
  unlist(lapply(source, function(x) gsub("^\t\t", "", x)))
}

TopLevel$examples_run <- function(., path = NULL, verbose=TRUE) {
  if (!.$doc) return(FALSE)
  set.seed(141079)

  require(decumar, quiet=TRUE, warn=FALSE)
  parsed <- nice_parse(.$examples_text())
  plots <- Filter(function(x) inherits(x$value, "ggplot") && x$visible, parsed)
  
  display <- function(x) {
    if (verbose) cat(x$src, "\n")
    if (is.null(path)) {
      print(x$value)
    } else {      
      ggsave(x$value, path=path)
    }
    x$src
  }
  out <- tryapply(plots, display)
  names(out) <- sapply(plots, function(x) digest.ggplot(x$value))
  
  invisible(out)
}


TopLevel$all_examples_run <- function(., path=NULL, verbose=TRUE) {
  old_opt <- options(warn = 1)
  on.exit(options(old_opt))
  
  out <- tryapply(.$find_all(), function(x) {
    if (verbose) message("Running examples for", " ", x$my_name())
    x$examples_run(path, verbose)
  })
  
  invisible(unlist(unname(out), recursive = FALSE))
}

# Run all examples
all_examples_run <- function(path=NULL, verbose = TRUE) {
  Geom$all_examples_run(path, verbose)
  Stat$all_examples_run(path, verbose)
  Scale$all_examples_run(path, verbose)
  Coord$all_examples_run(path, verbose)
  Position$all_examples_run(path, verbose)
  Facet$all_examples_run(path, verbose)
}

TopLevel$examples_profile <- function(.) {
  sw <- stopwatch(.$examples_run())
  plotting <- Filter(function(x) any(x == "\"print.ggplot\""), sw)
  attributes(plotting) <- attributes(sw)
  plotting
}
# print(plotting, 15, 4)

# Trim call tree to start with specified function
trim <- function(calltree, f) {
  trimmed <- compact(lapply(calltree, function(x) {
    if (!any(x == f)) return(NULL)
    tail(x, -(which(x == f)[1] - 1))
  }))
  attributes(trimmed) <- attributes(calltree)
  trimmed
}


TopLevel$examples_time <- function(.) {
  system.time(.$examples_run(verbose=FALSE))
}
TopLevel$all_examples_time <- function(.) {
  tryapply(.$find_all(), function(x) x$examples_time())
}
# t(sapply(Geom$all))

save_examples <- function() {
  rev <- get_rev(".")
  path <- paste("~/documents/ggplot/examples/ex-", rev, "/", sep="")
  dir.create(path, recursive = T)
  
  # Facet$all_examples_run(path, verbose = FALSE)
  GeomPoint$examples_run(path, verbose = FALSE)
  system(paste("pdf2png ", path, "*.pdf", sep =""))
  system(paste("rm ", path, "*.pdf", sep =""))
  
}

get_rev <- function(path = ".") {
  cmd <- paste("svn info ", path, "| grep 'Revision'")
  out <- system(cmd, intern=T)
  strsplit(out, " ")[[1]][2]
}
