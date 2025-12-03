# modify_list() -----------------------------------------------------------

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
  expect_snapshot(modifyList(testlist, testappend), error = TRUE)
})

test_that("modify_list retains unreferenced elements", {
  res <- modify_list(testlist, testappend)
  expect_equal(testlist$a, res$a)
})
test_that("modify_list overwrites existing values", {
  res <- modify_list(testlist, testappend)
  expect_equal(res$b, testappend$b)
})
test_that("modify_list adds new values", {
  res <- modify_list(testlist, testappend)
  expect_equal(res$d, testappend$d)
})
test_that("modify_list erases null elements", {
  res <- modify_list(testlist, testappend)
  expect_null(res$c)
  expect_named(res, c('a', 'b', 'd'))
})
