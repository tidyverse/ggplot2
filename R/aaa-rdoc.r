# Name of physical file to create, doesn't include directory
rdoc_path <- function(.) {
  ps(.$my_name(), ".rdoc")
}

all_rdoc_pages_create <- function(., path="web/") {
  invisible(lapply(.$find_all(), function(x) x$rdoc_page_create(path)))
}
  
rdoc_page_create <- function(., path="man/") {
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
    .$rdoc_advice(), 
    # .$rdoc_usage(),
    .$rdoc_arguments(),
    .$rdoc_seealso(),
    .$rdoc_value(),
    .$rdoc_examples(),
    .$rdoc_author(),
    .$rdoc_keywords(),
    ""
  )
}

TopLevel$rdoc_name <- function(.) {
  ps(
    "\\name{", firstUpper(.$class()), firstUpper(.$objname), "}\n"
  )
}

TopLevel$rdoc_aliases <- function(.) {
  ps(
    "\\alias{", .$my_name(), "}\n"
  )
}

TopLevel$rdoc_title <- function(.) {
  ps(
    "\\title{", .$my_name(), "}\n"
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
    rdoc_auto_link(ps("This page describes ", .$my_name(), ", see layer and qplot for how to create a complete plot from individual components.\n")),
    "}\n"
  )
}

TopLevel$rdoc_advice <- function(.) {
  if (.$advice == "") return()
  ps(
    "\\note{\n",
    rdoc_from_html(.$advice, .$my_name()),
    "}\n"
  )
}  

TopLevel$rdoc_arguments <- function(.) {
  p <- c("mapping", "data", "stat", "position", names(.$params()), "...")
  
  ps(
    "\\arguments{\n",
      ps(" \\item{", p, "}{", defaults(.$desc_params, .desc_param)[p], "}\n"),
    "}\n"
  )
}  


TopLevel$rdoc_seealso <- function(.) {
  ps(
    "\\seealso{\\itemize{\n",
    ps("  \\item ", rdoc_auto_link(names(.$seealso)), ": ", .$seealso, "\n"),
    "  \\item \\url{http://had.co.nz/ggplot/", .$html_path(), "}\n",
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
TopLevel$rdoc_keywords <- function(.) {
  "\\keyword{hplot}\n"
}  

rdoc_auto_link <- function(input, skip="") {
  if (!exists("links")) html_autolink_index()
  
  for (n in names(links)[names(links) != skip]) {
    input <- gsub(ps("\\b", n, "\\b"), ps("\\\\code{\\\\link{", n, "}}"), input)
  }
  input
}

rdoc_from_html <- function(html, skip="") {
  rd <- gsub("<p>", "", html)
  rd <- gsub("</p>\n?", "\n\n", rd)

  rd <- gsub("<em>(.*?)</em>", "\\\\emph{\\1}", rd)
  rd <- gsub("<code>(.*?)</code>", "\\\\code{\\1}", rd)
  
  rdoc_auto_link(rd, skip)
}
