context("Themes")

test_that("Adding opts() to themes", {
  # Changing a "leaf node" works
  t <- theme_grey() + opts(axis.title.x = element_text(colour='red'))
  expect_identical(t$axis.title.x, element_text(colour='red'))
  # Make sure the class didn't change or get dropped
  expect_true(is.theme(t))

  # Changing an intermediate node works
  t <- theme_grey() + opts(axis.title = element_text(colour='red'))
  expect_identical(t$axis.title, element_text(colour='red'))
  # Descendent is unchanged
  expect_identical(t$axis.title.x, theme_grey()$axis.title.x)

  # Adding empty opts() has no effect
  t <- theme_grey() + opts()
  expect_identical(t, theme_grey())

  expect_error(theme_grey() + "asdf")
})



test_that("Calculating theme element inheritance", {
  t <- theme_grey() + opts(axis.title = element_text(colour='red'))

  # Check that properties are passed along from axis.title to axis.title.x
  e <- calc_element('axis.title.x', t)
  expect_identical(e$colour, 'red')
  expect_false(is.null(e$family))
  expect_false(is.null(e$face))
  expect_false(is.null(e$size))


  # Check that rel() works for relative sizing, and is applied at each level
  t <- theme_grey(base_size=12) +
    opts(axis.title   = element_text(size=rel(0.5))) +
    opts(axis.title.x = element_text(size=rel(0.5)))
  e <- calc_element('axis.title', t)
  expect_identical(e$size, 6)
  ex <- calc_element('axis.title.x', t)
  expect_identical(ex$size, 3)
})
