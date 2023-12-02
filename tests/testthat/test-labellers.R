test_that("label_bquote has access to functions in the calling environment", {
  labels <- data.frame(lab = letters[1:2])
  attr(labels, "facet") <- "wrap"
  labeller <- label_bquote(rows = .(paste0(lab, ":")))
  labels_calc <- labeller(labels)
  expect_equal(labels_calc[[1]][[1]], "a:")
})

test_that("resolve_labeller() provide meaningful errors", {
  expect_snapshot_error(resolve_labeller(NULL, NULL))
  expect_snapshot_error(resolve_labeller(prod, sum, structure(1:4, facet = "wrap")))
})

test_that("labeller function catches overlap in names", {
  p <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point() +
    facet_grid(
      vs + am ~ gear,
      labeller = labeller(.rows = label_both, vs = label_value)
    )
  expect_snapshot_error(ggplotGrob(p))
})

test_that("labeller handles badly specified labels from lookup tables", {
  df <- data_frame0(am = c(0, 1))
  labs <- labeller(am = c("0" = "Automatic", "11" = "Manual"))
  expect_equal(labs(df), list(am = c("Automatic", "1")))
})

test_that("labeller allows cherry-pick some labels", {
  df <- data_frame0(am = c(0, 1))
  labs <- labeller(am = c("0" = "Automatic"))
  expect_equal(labs(df), list(am = c("Automatic", "1")))
})
