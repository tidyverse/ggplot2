Coord <- proto(TopLevel, expr={
  limits <- list()
  class <- function(.) "coord"
  
  muncher <- function(.) FALSE
  
  # Rescaling at coord level should not be clipped: this is what 
  # makes zooming work
  rescale_var <- function(., data, range) {
    rescale(data, 0:1, range, clip = FALSE)  
  }
  
  munch <- function(., data, details, npieces=50) {
    if (!.$muncher()) {
      .$transform(data, details)
    } else {
      data <- add_group(data)

      groups <- split(data, data$group)
      munched_groups <- lapply(groups, function(df) .$munch_group(df, details, npieces))
      do.call("rbind", munched_groups)      
    }
  }
  
  munch_group <- function(., data, details, npieces=50) {
    n <- nrow(data)

    x <- approx(data$x, n = npieces * (n - 1) + 1)$y
    y <- approx(data$y, n = npieces * (n - 1) + 1)$y
    
    cbind(
      .$transform(data.frame(x=x, y=y), details),
      data[c(rep(1:(n-1), each=npieces), n), setdiff(names(data), c("x", "y"))]
    )
  }
  
  pprint <- function(., newline=TRUE) {
    args <- formals(get("new", .))
    args <- args[!names(args) %in% c(".", "...")]
  
    cat("coord_", .$objname, ": ", clist(args), sep="")
    
    if (newline) cat("\n") 
  }
  
  guide_foreground <- function(., scales, theme) {
    theme_render(theme, "panel.border")
  }  
  # Html defaults
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a coordinate system object.</p>"
    )
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c("."))]
  }
  
})
