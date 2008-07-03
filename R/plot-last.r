.plot_store <- function() {
  .last_plot <- NULL
  
  list(
    get = function() .last_plot, 
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()

# Set last plot
# Set last plot created or modified
# 
# @arguments plot to store
# @keyword internal
set_last_plot <- function(value) .store$set(value)


# Retrieve last plot modified/created.
# Whenever a plot is created or modified, it is recorded.
# 
# @seealso \code{\link{ggsave}}
# @keyword hplot
last_plot <- function() .store$get()