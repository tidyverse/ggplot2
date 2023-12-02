test_that("aesthetic checking in geom throws correct errors", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = after_scale(data)))
  expect_snapshot_error(ggplotGrob(p))

  aes <- list(a = 1:4, b = letters[1:4], c = TRUE, d = 1:2, e = 1:5)
  expect_snapshot_error(check_aesthetics(aes, 4))
})



test_that("updating geom aesthetic defaults preserves class and order", {

  original_defaults <- GeomPoint$default_aes

  update_geom_defaults("point", aes(color = "red"))

  updated_defaults <- GeomPoint$default_aes

  expect_s3_class(updated_defaults, "uneval")

  intended_defaults <- original_defaults
  intended_defaults[["colour"]] <- "red"

  expect_equal(updated_defaults, intended_defaults)

  update_geom_defaults("point", original_defaults)

})




test_that("updating stat aesthetic defaults preserves class and order", {

  original_defaults <- StatBin$default_aes

  update_stat_defaults("bin", aes(y = after_stat(density)))

  updated_defaults <- StatBin$default_aes

  expect_s3_class(updated_defaults, "uneval")

  intended_defaults <- original_defaults
  intended_defaults[["y"]] <- expr(after_stat(density))
  attr(intended_defaults[["y"]], ".Environment") <- attr(updated_defaults[["y"]], ".Environment")

  expect_equal(updated_defaults, intended_defaults)

  update_stat_defaults("bin", original_defaults)

})
