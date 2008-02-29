TopLevel$examples <- function(.) {
  # Coming soon
}

TopLevel$examples_text <- function(.) {
  source <- attr(get("examples", .), "source")
  source <- source[-c(1, length(source))]
  
  unlist(lapply(source, function(x) gsub("^\t\t", "", x)))
}

TopLevel$examples_run <- function(., path = NULL, verbose=TRUE) {
  if (!.$doc) return(NULL)
  # Set seed to ensure reproducibility of examples with random components,
  # e.g. jittering
  set.seed(141079)

  require(decumar, quiet=TRUE, warn=FALSE)
  quiet <- if (verbose) force else suppressMessages
  parsed <- quiet(nice_parse(.$examples_text()))
  plots <- Filter(function(x) inherits(x$value, "ggplot") && x$visible, parsed)
  
  display <- function(x) {
    hash <- digest.ggplot(x$value)
    if (verbose) cat(x$src, "\n")
    if (is.null(path)) {
      timing <- try_default(system.time(print(x$value)), c(NA, NA, NA))
    } else {      
      timing <- try_default(system.time(ggsave(x$value, path=path, width=10, height=10)), c(NA, NA, NA))
    }
    timing <- unname(timing)
    data.frame(
      class = .$class(),
      obj = .$objname,
      src = x$src,
      hash = hash,
      user = timing[1],
      sys = timing[2],
      elapsed = timing[3],
      stringsAsFactors = FALSE
    )
  }
  out <- lapply(plots, display)
  invisible(do.call("rbind", out))
}

TopLevel$all_examples_run <- function(., path=NULL, verbose=TRUE) {
  # Ensure warnings display immediately
  old_opt <- options(warn = 1)
  on.exit(options(old_opt))
  
  out <- tryapply(.$find_all(), function(x) {
    if (verbose) message("Running examples for", " ", x$my_name())
    suppressMessages(x$examples_run(path, verbose))
  })
  
  invisible(do.call("rbind", compact(out)))
}

# Run all examples
# 
# @keyword internal
all_examples_run <- function(path=NULL, verbose = TRUE) {
  invisible(rbind(
    Geom$all_examples_run(path, verbose),
    Stat$all_examples_run(path, verbose),
    Scale$all_examples_run(path, verbose),
    Coord$all_examples_run(path, verbose),
    Position$all_examples_run(path, verbose),
    Facet$all_examples_run(path, verbose)
  ))
}


# Save all examples in consistent format -------------------------------------

# Save examples
# Cache output from all examples in ggplot directory
# 
# Produces:
#  * png for each example
#  * csv with hash, code and timing info
# 
# @keyword internal
save_examples <- function(name = get_rev("."), verbose = FALSE) {
  path <- paste("~/documents/ggplot/examples/ex-", name, "/", sep="")
  dir.create(path, recursive = TRUE)
  
  info <- all_examples_run(path, verbose = verbose)
  write.table(info, file=file.path(path, "info.csv"), sep=",",col=TRUE, row=FALSE, qmethod="d")
  system(paste("pdf2png ", path, "*.pdf", sep =""))
  system(paste("rm ", path, "*.pdf", sep =""))
  
  invisible(info)
}

# Get current revision
# Developer use only
# 
# @keyword internal
get_rev <- function(path = ".") {
  system(paste("svn up ", path), intern=T)
  cmd <- paste("svn info ", path, "| grep 'Revision'")
  out <- system(cmd, intern=T)
  strsplit(out, " ")[[1]][2]
}


# Profiling code -------------------------------------------------------------

TopLevel$examples_profile <- function(.) {
  sw <- stopwatch(.$examples_run())
  plotting <- Filter(function(x) any(x == "\"print.ggplot\""), sw)
  attributes(plotting) <- attributes(sw)
  plotting
}

# Trim call tree to start with specified function
# 
# @keyword internal
trim <- function(calltree, f) {
  trimmed <- compact(lapply(calltree, function(x) {
    if (!any(x == f)) return(NULL)
    tail(x, -(which(x == f)[1] - 1))
  }))
  attributes(trimmed) <- attributes(calltree)
  trimmed
}
