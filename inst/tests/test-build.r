# Test the complete path from plot specification to rendered data
context("Plot building")

pdata <- function(x) ggplot_build(x)$data

# Transform the data as the coordinate system does
cdata <- function(plot) {
  pieces <- ggplot_build(plot)
  
  lapply(pieces$data, function(d) {
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(pieces$panel, panel_data$PANEL[1])
      details <- plot$coord$compute_ranges(scales)
      plot$coord$transform(panel_data, details)
    })
  })
}

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("there is one data frame for each layer", {  
  l1 <- ggplot(df, aes(x, y)) + geom_point() 
  l2 <- ggplot(df, aes(x, y)) + geom_point() + geom_line() 
  l3 <- ggplot(df, aes(x, y)) + geom_point() + geom_line() + geom_point()

  expect_that(length(pdata(l1)), equals(1))
  expect_that(length(pdata(l2)), equals(2))
  expect_that(length(pdata(l3)), equals(3))
})

test_that("position aesthetics coerced to correct type", {
  l1 <- ggplot(df, aes(x, y)) + geom_point()
  d1 <- pdata(l1)[[1]]
  
  expect_that(d1$x, is_a("numeric"))
  expect_that(d1$y, is_a("numeric"))

  l2 <- ggplot(df, aes(x, z)) + geom_point() + scale_x_discrete()
  d2 <- pdata(l2)[[1]]

  expect_that(d2$x, is_a("integer"))
  expect_that(d2$y, is_a("integer"))  
})

test_that("non-position aesthetics are mapped", {
  l1 <- ggplot(df, aes(x, y, fill = z, colour = z, shape = z, size = z)) +
    geom_point() 
  d1 <- pdata(l1)[[1]]
  
  expect_that(sort(names(d1)), 
    equals(sort(c("x", "y", "fill", "colour", "shape", "size", "PANEL"))))

  l2 <- l1 + scale_colour_manual(values = c("blue", "red", "yellow"))
  d2 <- pdata(l2)[[1]]
  expect_that(d2$colour, equals(c("blue", "red", "yellow")))  
})

test_that("facets split up the data", {
  l1 <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~ z)
  d1 <- pdata(l1)[[1]]
  
  expect_that(d1$PANEL, equals(factor(1:3)))

  l2 <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(. ~ z)
  l3 <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(z ~ .)
  
  d2 <- pdata(l2)[[1]]
  d3 <- pdata(l3)[[1]]
  
  expect_that(d2, equals(d3))
  expect_that(sort(names(d2)), equals(sort(c("x", "y", "PANEL"))))
  expect_that(d2$PANEL, equals(factor(1:3)))
})



test_that("facets with free scales scale independently", {
  l1 <- ggplot(df, aes(x, y)) + geom_point() + 
    facet_wrap(~ z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_that(length(unique(d1$x)), equals(1))
  expect_that(length(unique(d1$y)), equals(1))

  l2 <- ggplot(df, aes(x, y)) + geom_point() + 
    facet_grid(. ~ z, scales = "free")
  d2 <- cdata(l2)[[1]]
  expect_that(length(unique(d2$x)), equals(3))
  expect_that(length(unique(d2$y)), equals(1))
  
  l3 <- ggplot(df, aes(x, y)) + geom_point() + 
    facet_grid(z ~ ., scales = "free")
  d3 <- cdata(l3)[[1]]
  expect_that(length(unique(d3$x)), equals(1))
  expect_that(length(unique(d3$y)), equals(3))
})
