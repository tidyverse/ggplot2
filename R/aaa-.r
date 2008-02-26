require("proto")
require("grid")
require("reshape")

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
    get(paste(firstUpper(.$class()), firstUpper(name), sep=""))
  }
  
  my_name <- function(., prefix=TRUE) {
    if (!prefix) return(.$objname)
    paste(.$class(), .$objname, sep="_")
  }
  
  myName <- function(.) {
    ps(firstUpper(.$class()), ps(firstUpper(strsplit(.$objname, "_")[[1]])))
  }

  
  doc <- TRUE
  
  # Function for html documentation ------------------------------------
  desc <- ""
  details <- ""
  advice <- ""
  objname <- ""
  desc_params <- list("..." = "ignored ")
  icon <- function(.) rectGrob(gp=gpar(fill="white", col=NA))
  
  # Name of physical file to create, doesn't include directory
  html_path <- function(.) {
    ps(.$my_name(), ".html")
  }
  
  html_link_self <- function(., prefix=TRUE) {
    ps("<a href='", .$html_path(), "' title='", .$desc, "'>", .$my_name(prefix=prefix), "</a>")
  }

  html_abbrev_link_self <- function(., prefix=TRUE) {
    ps("<a href='", .$html_path(), "' title='", .$desc, "'>", .$objname, "</a>")
  }

  html_parent_link <- function(.) {
    parent <- parent.env(.)
    if (identical(parent, TopLevel)) return("")
    ps(parent$html_parent_link(), " &gt; ", parent$html_link_self())
  }
  
  all_html_pages_create <- function(., path="web/") {
    invisible(lapply(.$find_all(), function(x) x$html_page_create(path)))
  }
    
  html_page_create <- function(., path="web/") {
    cat("Creating html documentation for", .$my_name(), "\n")
    target <- ps(path, .$html_path())
    
    .$html_img_draw(path)
    cat(.$html_page(), file=target)
  }  
    
  html_page <- function(.) {
    ps(
      .$html_header(),
      .$html_head(),
      .$html_details(),
      .$html_advice(),
      .$html_feedback(),
      .$html_aesthetics(),
      .$html_outputs(),
      .$html_parameters(),
      # .$html_defaults(),
      .$html_returns(),
      .$html_seealso(),
      .$html_examples(),
      .$html_feedback(),
      .$html_footer()
    )
  }
  
  # Header and footer templates -----------------------  
  html_header <- function(., title = .$my_name()) {
    template <- ps(readLines("templates/header.html"), collapse="\n")
    gsub("TITLE", title, template)
  }  

  html_footer <- function(.) {
    ps(readLines("templates/footer.html"), collapse="\n")
  }  
  
  # Page header -----------------------
  html_head <- function(.) {
    ps(
      # "<p class='hierarchy'>", .$html_parent_link(), "</p>\n",
      "<h1>", .$html_img(), .$my_name(), "</h1>\n",
      "<p class='call'>", .$html_call(), "</p>\n"
    )
  }
  
  html_details <- function(.) {
    ps(
      # "<h2>Details</h2>\n",
      "<div class='details'>\n",
      "<p>", .$desc, "</p>\n",
      html_auto_link(.$details, .$my_name()),
      "<p>This page describes ", .$my_name(), ", see <a href='layer.html'>layer</a> and <a href='qplot.html'>qplot</a> for how to create a complete plot from individual components.</p>\n",
      "</div>\n"
    )
  }

  html_advice <- function(.) {
    if (.$advice == "") return()
    ps(
      "<h2>Advice</h2>\n",
      "<div class='details'>\n",
      html_auto_link(.$advice, .$my_name()),
      "</div>\n"
    )
  }  

  html_scales <- function(., aesthetic) {
    scales <- Scale$find(aesthetic, only.documented = TRUE)
    if (length(scales) == 0) return()
    ps(lapply(scales, function(x) x$html_link_self(prefix=FALSE)), collapse=", ")
  }
  
  html_aesthetics <- function(.) {
    if (!exists("default_aes", .)) return("")
    
    req <- rep("<strong>required</strong>", length(.$required_aes))
    names(req) <- .$required_aes 
    
    aes <- c(req, .$default_aes())
    if (length(aes) == 0) return("")

    scale_links <- sapply(names(aes), .$html_scales)
    scale_links <- sapply(scale_links, function(x) {
      if (!is.null(x)) {
        ps("(scales: ", x, ")")
      } else {
        ""
      }
    })

    ps(
      "<h2>Aesthetics</h2>\n",
      html_auto_link(ps("<p>The following aesthetics can be used with  ", .$my_name(), ".  Aesthetics are mapped to variables in the data with the aes function: <code>", .$my_name(), "(aes(x = var))</code>. Scales control how the variable is mapped to the aesthetic and are listed after each aesthetic.</p>\n"), .$my_name()),
      "<ul>\n",
      ps(
        "  <li>", 
        "<p>", names(aes), ": <code>", aes, "</code> <span class='linklist'>", scale_links, "</span></p>", 
        "</li>\n"
      ),
      "</ul>\n"
    )
  }
  
  html_feedback <- function(.) {
    ps("<p class='feedback'>What do you think of the documentation?  <a href='http://hadley.wufoo.com/forms/documentation-feedback/default/field0/", .$my_name(), "'>Please let me know by filling out this short online survey</a>.</p>")
  }
  
  html_outputs <- function(.) {
    if (!exists("desc_outputs", .)) return("")
    
    ps(
      "<h2>New variables produced by the statistic</h2>\n",
      "<p>To use these variables in an aesthetic mapping, you need to surrond them with .., like <code>aes(x = ..output..)</code>. This tells ggplot that the variable isn't the original dataset, but has been created by the statistic.</p>\n",
      "<ul>\n",
      ps("<li><code>", names(.$desc_outputs), "</code>, ", .$desc_outputs, "</li>\n"),
      "</ul>\n"
    )
  }

  html_defaults <- function(.) {
    ps(
      "<h2>Defaults</h2>\n",
      "<ul>\n",
      .$html_defaults_stat(),
      .$html_defaults_geom(),
      .$html_defaults_position(),
      "</ul>\n"
    )
  }

  
  html_defaults_stat <- function(.) {
    if (!exists("default_stat", .)) return("")
    
    ps(
      "<li>", .$default_stat()$html_link_self(), ".  Override with the <code>stat</code> argument: <code>", .$my_name(), "(stat=\"identity\")</code></li>\n"
    )
  }
  
  html_defaults_geom <- function(.) {
    if (!exists("default_geom", .)) return("")
    
    ps(
      "<li>", .$default_geom()$html_link_self(), ".  Override with the  <code>geom</code> argument: <code>", .$my_name(), "(geom=\"point\")</code>.</li>\n"
    )
  }
  
  html_defaults_position <- function(.) {
    if (!exists("default_pos", .)) return("")
    
    ps(
      "<li>", .$default_pos()$html_link_self(), ".  Override with the <code>position</code> argument: <code>", .$my_name(), "(position=\"jitter\")</code>.</li>\n"
    )
  }
  
  params <- function(.) {
    param <- .$parameters()
    if (length(param) == 0) return()
  
    if(!exists("required_aes", .)) return(param)
  
    aesthetics <- c(.$required_aes, names(.$default_aes()))
    param <- param[setdiff(names(param), aesthetics)]
  }
  
  
  html_parameters <- function(.) {
    if (!exists("parameters", .)) return("")
    param <- .$params()
    
    ps(
      "<h2>Parameters</h2>\n",
      "<p>Parameters control the appearance of the ", .$class(), ". In addition to the parameters listed below (if any), any aesthetic can be used as a parameter, in which case it will override any aesthetic mapping.</p>\n",
      if(length(param) > 0) ps(
        "<ul>\n",
        ps("<li><code>", names(param), "</code>: ", defaults(.$desc_params, .desc_param)[names(param)], "</li>\n"),
        "</ul>\n"
      )
    )
  }
  
  # See also ---------------------------
  
  seealso <- list()
  html_seealso <- function(.) {
    if (length(.$seealso) == 0) return()
    ps(
      "<h2>See also</h2>",
      "<ul>\n",
      ps("<li>", html_auto_link(names(.$seealso)), ": ", .$seealso, "</li>\n"),
      "</ul>\n"
    )
  }

  # Returns ---------------------------

  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a <a href='layer.html'>layer</a> object.</p>"
    )
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
    require(decumar, quiet=TRUE, warn=FALSE)
    if (!.$doc) return(FALSE)
    
    if (length(parse(text = .$examples_text())) == 0) {
      output <- paste("> ", highlight.html(.$examples_text()), collapse="\n")
      return(ps("<h2>Examples</h2>\n", html_auto_link(output)))
    }
      
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd("~/Documents/ggplot/ggplot/web/")
    INCLUDES <<- "graphics"
    
    parsed <- nice_parse(.$examples_text())
    html_auto_link(ps(
      "<h2>Examples</h2>\n",
      ps(capture.output(create_html(parsed)),collapse="\n"),
      "\n"
    ), .$my_name())
  }
  
  
  html_call <- function(.) {
    ps(
      .$my_name(), "(",
      ps(
        "mapping&nbsp;=&nbsp;aes(...)", 
        "data&nbsp;=&nbsp;NULL",
        if(exists("default_stat", .))
          ps("stat&nbsp;=&nbsp;'", .$default_stat()$html_abbrev_link_self(), "'"),
        if(exists("default_geom", .))
          ps("geom&nbsp;=&nbsp;'", .$default_geom()$html_abbrev_link_self(), "'"),
        if(exists("default_pos", .))
          ps("position&nbsp;=&nbsp;'", .$default_pos()$html_abbrev_link_self(), "'"),
        plist(.$params()), 
        "...",
        sep = ", "
      ), 
      ")"
    )
  }
  
})

print.proto <- function(x, ...) x$pprint(...)
pprint <- function(x, ...) print(as.list(x), ...)
# name.proto <- function (...) {
#        proto(print.proto = print.default, f = proto::name.proto)$f(...)
# }


