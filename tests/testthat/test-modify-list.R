context("modify_list")

testlist <- list(
  a = 5.5,
  b = "x",
  c = 1:10
)
testappend <- list(
  b = "y",
  c = NULL,
  d = FALSE
)

test_that("modifyList is masked", {
  expect_error(modifyList(testlist, testappend))
})

test_that("modify_list does what it should", {
  res <- modify_list(testlist, testappend)
  expect_equal(testlist$a, res$a)
  expect_equal(res$b, testappend$b)
  expect_null(res$c)
  expect_equal(res$d, testappend$d)
  expect_named(res, c('a', 'b', 'd'))
})
