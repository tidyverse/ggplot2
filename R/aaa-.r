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
  
  html_aesthetics <- function(.) {
    if (!exists("default_aes", .)) return("")
    
    req <- rep("<strong>required</strong>", length(.$required_aes))
    names(req) <- .$required_aes 
    
    aes <- c(req, .$default_aes())
    if (length(aes) == 0) return("")

    scale_links <- sapply(names(aes), .$html_scales)
    scale_links <- sapply(scale_links, ps)

    ps(
      "<h2>Aesthetics</h2>\n",
      html_auto_link(ps("<p>The following aesthetics can be used with  ", .$my_name(), ".  Aesthetics are mapped to variables in the data with the aes function: <code>", .$my_name(), "(aes(x = var))</code>. Note that you do not need quotes around the variable name.</p>\n",
      "<p>Scales control how the variable is mapped to the aesthetic and are listed after each aesthetic.</p>\n"), .$my_name()),
      "<table width='100%'>\n",
      "<tr><th>Aesthetic</th> <th>Default</th> <th>Related scales</th></tr>\n",
      ps(
        "<tr>\n", 
        "<td>", names(aes), "</td><td>", aes, "</td><td>", scale_links, "</td>\n", 
        "</tr>\n"
      ),
      "</table>\n",
      "<p>Layers are divided into groups by the <code>group</code> aesthetic.  By default this is set to the interaction of all categorical variables present in the plot.  </p>\n"
    )
  }
  
  html_feedback <- function(.) {
    ps("<p class='feedback'>What do you think of the documentation?  <a href='http://hadley.wufoo.com/forms/documentation-feedback/def/field0=", .$my_name(), "'>Please let me know by filling out this short online survey</a>.</p>")
  }
  

  
  # Object icon -----------------------
  html_img_path <- function(.) {
    ps(.$my_name(), ".png")
  }
  
  html_img_link <- function(., align=NULL) {
    ps("<a href='", .$html_path(), "'>", .$html_img(align), "</a>")
  }
  
  html_img <- function(., align=NULL) {
    ps(
      "<img src='", .$html_img_path(), "'", if (!is.null(align)) {ps(" align='", align, "'")}, " width='50' height='50' alt='' class='icon' />\n"
    )
  }
  
  html_img_draw <- function(., path="web/") {
    png(ps(path, .$html_img_path()), width=50, height=50)
    grid.newpage()
    grid.draw(.$icon())
    dev.off()
  }

  # Examples -----------------------
  html_examples <- function(.) {
    if (!.$doc) return(FALSE)
    require("decumar", quiet = TRUE)
    
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd("~/Documents/ggplot/ggplot/web")
    
    html_auto_link(ps(
      "<h2>Examples</h2>\n",
      interweave_html(.$examples_text(), outdir = "graphics", dpi = 72),
      "\n"
    ), .$my_name())
  }

})

#' @S3method print proto
print.proto <- function(x, ...) x$pprint(...)
pprint <- function(x, ...) print(as.list(x), ...)
# name.proto <- function (...) {
#        proto(print.proto = print.default, f = proto::name.proto)$f(...)
# }


