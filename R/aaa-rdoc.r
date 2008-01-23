# # Name of physical file to create, doesn't include directory
# rdoc_path <- function(.) {
#   ps(.$my_name(), ".rdoc")
# }
# 
# all_rdoc_pages_create <- function(., path="web/") {
#   invisible(lapply(.$find_all(), function(x) x$rdoc_page_create(path)))
# }
#   
# rdoc_page_create <- function(., path="man/") {
#   cat("Creating rdoc documentation for", .$my_name(), "\n")
#   target <- ps(path, .$rdoc_path())
#   cat(.$rdoc_page(), file=target)
# }  
#   
# rdoc_page <- function(.) {
#   ps(
#     .$rdoc_name(),
#     .$rdoc_aliases(),
#     .$rdoc_title(), 
#     .$rdoc_description(), 
#     .$rdoc_details(), 
#     .$rdoc_advice(), 
#     .$rdoc_usage(),
#     .$rdoc_arguments(),
#     .$rdoc_seealso(),
#     .$rdoc_examples(),
#     .$rdoc_keywords()
#   )
# }
# 
# rdoc_description <- function(.) {
#   ps(
#     "\\description{", .$desc, "}\n",
#   )
# }
# 
# rdoc_details <- function(.) {
#   ps(
#     "\\details{\n",
#     rdoc_auto_link(.$details, .$my_name()),
#     "<p>This page describes ", .$my_name(), ", see <a href='layer.rdoc'>layer</a> and <a href='qplot.rdoc'>qplot</a> for how to create a complete plot from individual components.</p>\n",
#     "}\n"
#   )
# }
# 
# rdoc_advice <- function(.) {
#   if (.$advice == "") return()
#   ps(
#     "\\note{\n",
#     rdoc_auto_link(.$advice, .$my_name()),
#     "}\n"
#   )
# }  
# 
# rdoc_scales <- function(., aesthetic) {
#   scales <- Scale$find(aesthetic, only.documented = TRUE)
#   if (length(scales) == 0) return()
#   ps(lapply(scales, function(x) x$rdoc_link_self(prefix=FALSE)), collapse=", ")
# }
# 
# rdoc_aesthetics <- function(.) {
#   if (!exists("default_aes", .)) return("")
#   
#   req <- rep("<strong>required</strong>", length(.$required_aes))
#   names(req) <- .$required_aes 
#   
#   aes <- c(req, .$default_aes())
#   if (length(aes) == 0) return("")
# 
#   scale_links <- sapply(names(aes), .$rdoc_scales)
#   scale_links <- sapply(scale_links, function(x) {
#     if (!is.null(x)) {
#       ps("(scales: ", x, ")")
#     } else {
#       ""
#     }
#   })
# 
#   ps(
#     "<h2>Aesthetics</h2>\n",
#     rdoc_auto_link(ps("<p>The following aesthetics can be used with  ", .$my_name(), ".  Aesthetics are mapped to variables in the data with the aes function: <code>", .$my_name(), "(aes(x = var))</code>. Scales control how the variable is mapped to the aesthetic and are listed after each aesthetic.</p>\n"), .$my_name()),
#     "<ul>\n",
#     ps(
#       "  <li>", 
#       "<p>", names(aes), ": <code>", aes, "</code> <span class='linklist'>", scale_links, "</span></p>", 
#       "</li>\n"
#     ),
#     "</ul>\n"
#   )
# }
# 
# rdoc_feedback <- function(.) {
#   ps("<p class='feedback'>What do you think of the documentation?  <a href='http://hadley.wufoo.com/forms/documentation-feedback/default/field0/", .$my_name(), "'>Please let me know by filling out this short online survey</a>.</p>")
# }
# 
# rdoc_outputs <- function(.) {
#   if (!exists("desc_outputs", .)) return("")
#   
#   ps(
#     "<h2>New variables produced by the statistic</h2>\n",
#     "<p>To use these variables in an aesthetic mapping, you need to surrond them with .., like <code>aes(x = ..output..)</code>. This tells ggplot that the variable isn't the original dataset, but has been created by the statistic.</p>\n",
#     "<ul>\n",
#     ps("<li><code>", names(.$desc_outputs), "</code>, ", .$desc_outputs, "</li>\n"),
#     "</ul>\n"
#   )
# }
# 
# rdoc_defaults <- function(.) {
#   ps(
#     "<h2>Defaults</h2>\n",
#     "<ul>\n",
#     .$rdoc_defaults_stat(),
#     .$rdoc_defaults_geom(),
#     .$rdoc_defaults_position(),
#     "</ul>\n"
#   )
# }
# 
# 
# rdoc_defaults_stat <- function(.) {
#   if (!exists("default_stat", .)) return("")
#   
#   ps(
#     "<li>", .$default_stat()$rdoc_link_self(), ".  Override with the <code>stat</code> argument: <code>", .$my_name(), "(stat=\"identity\")</code></li>\n"
#   )
# }
# 
# rdoc_defaults_geom <- function(.) {
#   if (!exists("default_geom", .)) return("")
#   
#   ps(
#     "<li>", .$default_geom()$rdoc_link_self(), ".  Override with the  <code>geom</code> argument: <code>", .$my_name(), "(geom=\"point\")</code>.</li>\n"
#   )
# }
# 
# rdoc_defaults_position <- function(.) {
#   if (!exists("default_pos", .)) return("")
#   
#   ps(
#     "<li>", .$default_pos()$rdoc_link_self(), ".  Override with the <code>position</code> argument: <code>", .$my_name(), "(position=\"jitter\")</code>.</li>\n"
#   )
# }
# 
# params <- function(.) {
#   param <- .$parameters()
#   if (length(param) == 0) return()
# 
#   if(!exists("required_aes", .)) return(param)
# 
#   aesthetics <- c(.$required_aes, names(.$default_aes()))
#   param <- param[setdiff(names(param), aesthetics)]
# }
# 
# 
# rdoc_parameters <- function(.) {
#   if (!exists("parameters", .)) return("")
#   param <- .$params()
#   
#   ps(
#     "<h2>Parameters</h2>\n",
#     "<p>Parameters control the appearance of the ", .$class(), ". In addition to the parameters listed below (if any), any aesthetic can be used as a parameter, in which case it will override any aesthetic mapping.</p>\n",
#     if(length(param) > 0) ps(
#       "<ul>\n",
#       ps("<li><code>", names(param), "</code>: ", defaults(.$desc_params, .desc_param)[names(param)], "</li>\n"),
#       "</ul>\n"
#     )
#   )
# }
# 
# # See also ---------------------------
# 
# seealso <- list()
# rdoc_seealso <- function(.) {
#   if (length(.$seealso) == 0) return()
#   ps(
#     "<h2>See also</h2>",
#     "<ul>\n",
#     ps("<li>", rdoc_auto_link(names(.$seealso)), ": ", .$seealso, "</li>\n"),
#     "</ul>\n"
#   )
# }
# 
# # Returns ---------------------------
# 
# rdoc_returns <- function(.) {
#   ps(
#     "<h2>Returns</h2>\n",
#     "<p>This function returns a <a href='layer.rdoc'>layer</a> object.</p>"
#   )
# }
# 
# # Object icon -----------------------
# rdoc_img_path <- function(.) {
#   ps(.$my_name(), ".png")
# }
# 
# rdoc_img_link <- function(., align=NULL) {
#   ps("<a href='", .$rdoc_path(), "'>", .$rdoc_img(align), "</a>")
# }
# 
# rdoc_img <- function(., align=NULL) {
#   ps(
#     "<img src='", .$rdoc_img_path(), "'", if (!is.null(align)) {ps(" align='", align, "'")}, " width='50' height='50' alt='' class='icon' />\n"
#   )
# }
# 
# rdoc_img_draw <- function(., path="web/") {
#   png(ps(path, .$rdoc_img_path()), width=50, height=50)
#   grid.newpage()
#   grid.draw(.$icon())
#   dev.off()
# }
# 
# # Examples -----------------------
# rdoc_examples <- function(.) {
#   require(decumar, quiet=TRUE, warn=FALSE)
#   if (!.$doc) return(FALSE)
#   
#   if (length(parse(text = .$examples_text())) == 0) {
#     output <- paste("> ", highlight.rdoc(.$examples_text()), collapse="\n")
#     return(ps("<h2>Examples</h2>\n", rdoc_auto_link(output)))
#   }
#     
#   curdir <- getwd()
#   on.exit(setwd(curdir))
#   setwd("~/Documents/ggplot/ggplot/web/")
#   INCLUDES <<- "graphics"
#   
#   parsed <- nice_parse(.$examples_text())
#   rdoc_auto_link(ps(
#     "<h2>Examples</h2>\n",
#     ps(capture.output(create_rdoc(parsed)),collapse="\n"),
#     "\n"
#   ), .$my_name())
# }
