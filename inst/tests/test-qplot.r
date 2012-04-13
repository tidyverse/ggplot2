context("qplot")

test_that("qplot works in non-standard environments", {
  env <- new.env(parent = globalenv())
  expr <- quote({
    `-1-` <- 10
    x <- 1:10
    qplot(x, breaks = 0:`-1-`)
  })
  
  expect_is(eval(expr, env), "ggplot")
  
})

