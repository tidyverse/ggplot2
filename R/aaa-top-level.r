# INCLUDES <- "web/graphics"
# FILETYPE <- "html"

# Upper case first letter of string
# 
# @keyword internal
firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="")
}

TopLevel <- proto(expr = {
  find_all <- function(., only.documented = FALSE) {
    names <- ls(pattern=paste("^", firstUpper(.$class()), "[A-Z].+", sep=""), .GlobalEnv)
    objs <- structure(lapply(names, get), names=names)
    
    if (only.documented) objs <- objs[sapply(objs, function(x) get("doc", x))]
    objs
  }
  find <- function(., name) {
    get(paste(firstUpper(.$class()), firstUpper(name), sep=""))
  }
  
  accessors <- function(.) create_accessors(.$find_all(), .$class())
  accessors_print <- function(.) invisible(lapply(.$accessors(), cat))

  examples <- function(.) {
    # Coming soon
  }

  examples_text <- function(.) {
    source <- attr(get("examples", .), "source")
    source <- source[-c(1, length(source))]
    
    unlist(lapply(source, function(x) gsub("^\t\t", "", x)))
  }

  examples_run <- function(.) {
    ggopt(auto.print=TRUE)

    tryCatch(
      .$examples(),
      finally = invisible(ggopt(auto.print=FALSE))
    )
  }

  all_examples_run <- function(.) {
    old_opt <- options(warn = 1)
    on.exit(options(old_opt))
    
    tryapply(.$find_all(), function(x) {
      cat("Running examples for", x$my_name(), "\n")
      x$examples_run()
    })
    invisible()
  }
  
  my_name <- function(., prefix=TRUE) {
    if (!prefix) return(.$objname)
    paste(.$class(), .$objname, sep="_")
  }
  
  doc <- TRUE
  
  # Function for html documentation ------------------------------------
  desc <- ""
  details <- ""
  objname <- ""
  desc_params <- list()
  icon <- function(.) rectGrob(gp=gpar(fill="white", col=NA))
  
  # Name of physical file to create, doesn't include directory
  html_path <- function(.) {
    ps(.$my_name(), ".html")
  }
  
  html_link_self <- function(., prefix=TRUE) {
    ps("<a href='", .$html_path(), "' title='", .$desc, "'>", .$my_name(prefix=prefix), "</a>")
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
      .$html_defaults(),
      .$html_returns(),
      .$html_seealso(),
      .$html_examples(),
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
      .$html_img(), 
      # "<p class='hierarchy'>", .$html_parent_link(), "</p>\n",
      "<h1>", .$my_name(), "</h1>\n",
      "<p class='desc'>", .$desc, "</p>\n"
    )
  }
  
  html_details <- function(.) {
    ps(
      "<h2>Details</h2>\n",
      "<div class='details'>\n",
      html_auto_link(.$details, .$my_name()),
      "<p>See <a href='layer.html'>layer</a> and <a href='qplot.html'>qplot</a> for more information on creating a complete plot from multiple components.</p>\n",
      "</div>\n"
    )
  }
  
  # Defaults -----------------------
  html_defaults <- function(.) {
    ps(
      .$html_defaults_aesthetics(),
      .$html_defaults_outputs(),
      .$html_defaults_parameters(),
      .$html_defaults_stat(),
      .$html_defaults_geom(),
      .$html_defaults_position()
    )
  }
  
  html_scales <- function(., aesthetic) {
    scales <- Scale$find(aesthetic, only.documented = TRUE)
    if (length(scales) == 0) return()
    ps(lapply(scales, function(x) x$html_link_self(prefix=FALSE)), collapse=", ")
  }
  
  html_defaults_aesthetics <- function(.) {
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
      html_auto_link(ps("<p>The following aesthetics can be used with  ", .$my_name(), ".  They are listed along with their default value.  All geoms and scales can also use the group aesthetic.  Read how this important aesthetic works in scale_group. Typically, you will associate an aesthetic with a variable in your data set.  To do this, you use the aes function: <code>", .$my_name(), "(aes(x = var))</code>. Scales control the details of the mapping between data and aesthetic properties; after each aesthetic are listed scales that can be used with that aesthetic. The scale documentation will also provide references to help you interpret the default values.</p>\n<p>Instead of mapping an aesthetic to a variable in your dataset, you can also set it to a fixed value.  See the parameters section for details.</p>\n"), .$my_name()),
      "<ul>\n",
      ps(
        "  <li>", 
        "<p>", names(aes), ": <code>", aes, "</code> <span class='linklist'>", scale_links, "</span></p>", 
        "</li>\n"
      ),
      "</ul>\n"
    )
  }
  
  html_defaults_outputs <- function(.) {
    if (!exists("desc_outputs", .)) return("")
    
    ps(
      "<h2>New variables produced by the statistic</h2>\n",
      "<p>To use these variables in an aesthetic mapping, you need to surrond them with .., like <code>aes(x = ..output..)</code>. This tells ggplot that the variable isn't the original dataset, but has been created by the statistic.</p>\n",
      "<ul>\n",
      ps("<li><code>", names(.$desc_outputs), "</code>, ", .$desc_outputs, "</li>\n"),
      "</ul>\n"
    )
  }
  
  html_defaults_stat <- function(.) {
    if (!exists("default_stat", .)) return("")
    
    ps(
      "<h2>Default statistic</h2>\n",
      "<p>", .$default_stat()$html_link_self(), ".  Override with the <code>stat</code> argument: <code>", .$my_name(), "(stat=\"identity\")</code></p>\n"
    )
  }
  
  html_defaults_geom <- function(.) {
    if (!exists("default_geom", .)) return("")
    
    ps(
      "<h2>Default geom</h2>\n",
      "<p>", .$default_geom()$html_link_self(), ".  Override with the  <code>geom</code> argument: <code>", .$my_name(), "(geom=\"point\")</code>.</p>\n"
    )
  }
  
  html_defaults_position <- function(.) {
    if (!exists("default_pos", .)) return("")
    
    ps(
      "<h2>Default position</h2>\n",
      "<p>", .$default_pos()$html_link_self(), ".  Override with the <code>position</code> argument: <code>", .$my_name(), "(position=\"jitter\")</code>.</p>\n"
    )
  }
  
  html_defaults_parameters <- function(.) {
    if (!exists("parameters", .)) return("")

    param <- .$parameters()
    if (length(param) == 0) return("")
    
    desc <- c(.$desc_params, .$.desc_aes)
    
    ps(
      "<h2>Parameters</h2>\n",
      "<p>When an aesthetic is used as an a parameter, like <code>", .$my_name(), "(", names(param)[1], " = 3)</code>, it will override mappings from data.</p>\n",
      "<ul>\n",
      ps("<li><code>", names(param), "</code>, ", desc[names(param)], "</li>\n"),
      "</ul>\n"
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
  
  html_img_link <- function(., align="right") {
    ps("<a href='", .$html_path(), "'>", .$html_img(align), "</a>")
  }
  
  html_img <- function(., align="right") {
    ps(
      "<img src='", .$html_img_path(), "' width='50' height='50' alt='' align='", align, "' class='icon' />\n"
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
  
  
})

print.proto <- function(x, ...) x$pprint(...)
# name.proto <- function (...) {
#        proto(print.proto = print.default, f = proto::name.proto)$f(...)
# }
pprint <- function(x, ...) print(as.list(x), ...)

