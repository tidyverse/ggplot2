context("Limits")

test_that("lower limit is not greater that upper for continuous scales", {
  df <- data.frame(x = 1:2, y = 2:1, col = 1:2)
  p <- ggplot(df, aes(x = x, y = y, col = col)) + 
    geom_point()
  
  parse_build_ggplot <- function(s) {
    ggplot_build(p + eval(parse(text = s)))
  }
  
  expect_limit_error <- function(s) {
    expect_error(parse_build_ggplot(s),
                 "Lower limit greater than upper for continuous scale")
  } 
  expect_limit_pass <- function(s) {
    expect_true({parse_build_ggplot(s); TRUE})
  }
  
  scale_aes <- c("x", "y", "colour", "alpha", "size")
  bad_scales_list <- as.list(paste0("scale_", 
                                    scale_aes,
                                    "_continuous(limits = test_limits)"
  ))

  test_limits <- c(1, -1)
  lapply(bad_scales_list, expect_limit_error)
  
  test_limits <- c(-1, 1)
  lapply(bad_scales_list, expect_limit_pass)
})
