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
  expect_error(modifyList(testlist, testappend))
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


# new_data_frame() --------------------------------------------------------

test_that("new_data_frame handles zero-length inputs", {
  # zero-length input creates zero-length data frame
  d <- new_data_frame(list(x = numeric(0), y = numeric(0)))
  expect_equal(nrow(d), 0L)

  # constants are ignored in the context of zero-length input
  d <- new_data_frame(list(x = numeric(0), y = numeric(0), z = 1))
  expect_equal(nrow(d), 0L)

  # vectors of length > 1 don't mix with zero-length input
  expect_error(
    new_data_frame(list(x = numeric(0), y = numeric(0), z = 1, a = c(1, 2))),
    "Elements must equal the number of rows or 1"
  )

  # explicit recycling doesn't work with zero-length input
  expect_error(
    new_data_frame(list(x = numeric(0), z = 1), n = 5),
    "Elements must equal the number of rows or 1"
  )
  # but it works without
  d <- new_data_frame(list(x = 1, y = "a"), n = 5)
  expect_equal(nrow(d), 5L)
  expect_identical(d$x, rep(1, 5L))
  expect_identical(d$y, rep("a", 5L))

})
