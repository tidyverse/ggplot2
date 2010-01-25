# Rebuild all rdoc documentation
# Builds rdoc pages for all ggplot2 objects
# 
# @arguments path to save rd files
# @keyword internal
all_rdoc_pages_create <- function(path="web/") {
  Geom$all_rdoc_pages_create()
  Stat$all_rdoc_pages_create()
  Scale$all_rdoc_pages_create()
  Coord$all_rdoc_pages_create()
  Position$all_rdoc_pages_create()
  Facet$all_rdoc_pages_create()
}

# Name of physical file to create, doesn't include directory
TopLevel$rdoc_path <- function (.) {
  ps(.$my_name(), ".rd")
}

TopLevel$all_rdoc_pages_create <- function(., path="man/") {
  invisible(lapply(.$find_all(TRUE), function(x) x$rdoc_page_create(path)))
}
  
TopLevel$rdoc_page_create <- function(., path="man/") {
  cat("Creating rdoc documentation for", .$my_name(), "\n")
  target <- ps(path, .$rdoc_path())
  cat(.$rdoc_page(), file=target)
}  
  
TopLevel$rdoc_page <- function(.) {
  ps(
    .$rdoc_name(),
    .$rdoc_aliases(),
    .$rdoc_title(), 
    .$rdoc_description(), 
    .$rdoc_details(), 
    .$rdoc_aesthetics(), 
    .$rdoc_advice(), 
    .$rdoc_usage(),
    .$rdoc_arguments(),
    .$rdoc_seealso(),
    .$rdoc_value(),
    .$rdoc_examples(),
    .$rdoc_author(),
    .$rdoc_keyword(),
    ""
  )
}

TopLevel$rdoc_name <- function(.) {
  ps(
    "\\name{", .$my_name(), "}\n"
  )
}

TopLevel$aliases <- c()

TopLevel$rdoc_aliases <- function(.) {
  aliases <- unique(c(
    .$my_name(),
    .$my_names(),
    .$myName(),
    .$aliases
  ))
  
  ps(
    "\\alias{", gsub("%", "\\%", aliases), "}\n"
  )
}

TopLevel$rdoc_title <- function(.) {
  ps(
    "\\title{", gsub("_", "\\\\_", .$my_name()), "}\n"
  )
}

TopLevel$rdoc_description <- function(.) {
  ps(
    "\\description{", .$desc, "}\n"
  )
}

TopLevel$rdoc_details <- function(.) {
  ps(
    "\\details{\n",
    rdoc_from_html(.$details, .$my_name()),
    rdoc_from_html(ps("This page describes ", .$my_name(), ", see layer and qplot for how to create a complete plot from individual components.\n")),
    "}\n"
  )
}

TopLevel$rdoc_aesthetics <- function(.) {
  if (!exists("default_aes", .)) return("")
  
  aes <- c(.$required_aes, names(.$default_aes()))
  if (length(aes) == 0) return("")

  req <- ifelse(aes %in% .$required_aes, " (\\strong{required})", "")
  desc <- paste(defaults(.$desc_params, .desc_aes)[aes], req, sep="")

  ps(
    "\\section{Aesthetics}{\n",
    rdoc_from_html(ps("The following aesthetics can be used with ", .$my_name(), ".  Aesthetics are mapped to variables in the data with the aes function: \\code{", .$my_name(), "(aes(x = var))}"), .$my_name()), "\n", 
    "\\itemize{\n",
    ps("  \\item \\code{", aes, "}: ", desc, " \n"), 
    "}\n",
    "}\n"
  )
}


TopLevel$rdoc_advice <- function(.) {
  if (.$advice == "") return()
  ps(
    "\\section{Advice}{\n",
    rdoc_from_html(.$advice, .$my_name()),
    "}\n"
  )
}  

TopLevel$rdoc_formals <- function(.)   {
  if (exists("common", .) && !is.null(.$common)) {
    formals(get(ps(.$class(), "_", .$common[1], "_", .$objname)))    
  } else {
    formals(get(.$my_name()))
  }
  
}



TopLevel$call <- function(.) {
  args <- .$rdoc_formals()
  is.missing.arg <- function(arg) sapply(arg, typeof) == "symbol" & sapply(arg, deparse) == ""

  equals <- ifelse(is.missing.arg(args), "", "=")
  ps(
    .$my_names(), ps("(", 
    ps(names(args), equals, sapply(args, deparse), collapse=", "),
    ")"), collapse=NULL
  )
}


# FIXME: need to generate usage statements for all common scales
TopLevel$rdoc_usage <- function (.) {
  # add line breaks
  call <- deparse(parse(text = .$call()))
  
  ps(
    "\\usage{", ps(call, collapse="\n"), "}\n"
  )
}

TopLevel$rdoc_arguments <- function(.) {
  p <- names(.$rdoc_formals())
  # p <- c("mapping", "data", "stat", "position", names(.$params()), "...")
  
  ps(
    "\\arguments{\n",
      ps(" \\item{", p, "}{", defaults(.$desc_params, .desc_param)[p], "}\n"),
    "}\n"
  )
}  


TopLevel$rdoc_seealso <- function(.) {
  ps(
    "\\seealso{\\itemize{\n",
    if(length(.$seealso) > 0) {
      ps("  \\item \\code{\\link{", names(.$seealso), "}}: ", .$seealso, "\n")
    },
    "  \\item \\url{http://had.co.nz/ggplot2/", .$html_path(), "}\n",
    "}}\n"
  )
}  

TopLevel$rdoc_value <- function(.) {
  "\\value{A \\code{\\link{layer}}}\n"
}  

TopLevel$rdoc_examples <- function(.) {
  if (!.$doc) return()
  
  ps(
    "\\examples{\\dontrun{\n",
    ps(.$examples_text(), collapse="\n"),
    "\n}}\n"
  )
}

TopLevel$rdoc_author <- function(.) {
  "\\author{Hadley Wickham, \\url{http://had.co.nz/}}\n"
}  
TopLevel$rdoc_keyword<- function(.) {
  "\\keyword{hplot}\n"
}  

# rdoc auto link
# Automatically link functions used in rdoc
# 
# @arguments input rdoc string
# @argument functions to omit
# @keyword internal
rdoc_auto_link <- function(input, skip="") {
  if (!exists("links")) html_autolink_index()
  
  for (n in names(links)[names(links) != skip]) {
    input <- gsub(ps("\\b", n, "\\b"), ps("\\\\code{\\\\link{", n, "}}"), input)
  }
  input
}

# Convert rdoc to html
# Crude regexp based conversion from html to rdoc.
# 
# Assumes well-formed xhtml.  Also autolinks any ggplot functions.
# 
# @arguments input rdoc string
# @arguments pass to \code{\link{rdoc_auto_link}}
# @keyword internal
rdoc_from_html <- function(html, skip="") {
  rd <- gsub("<p>", "", html)
  rd <- gsub("</p>\n?", "\n\n", rd)

  rd <- gsub("<em>(.*?)</em>", "\\\\emph{\\1}", rd)
  rd <- gsub("<code>(.*?)</code>", "\\\\code{\\1}", rd)

  rd <- gsub("_", "\\\\_", rd)
  
  rdoc_auto_link(rd, skip)
}
