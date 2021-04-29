context("Labellers")

test_that("label_bquote has access to functions in the calling environment", {
  labels <- data.frame(lab = letters[1:2])
  attr(labels, "facet") <- "wrap"
  labeller <- label_bquote(rows = .(paste0(lab, ":")))
  labels_calc <- labeller(labels)
  expect_equal(labels_calc[[1]][[1]], "a:")
})
