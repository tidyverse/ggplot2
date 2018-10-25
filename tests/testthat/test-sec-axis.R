context("sec-axis")

x <- exp(seq(log(0.001), log(1000), length.out = 100))
foo <- data.frame(
  x = x,
  y = x/(1+x)
)

test_that("dup_axis() works", {
  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_continuous(name = "Unit A",
                       sec.axis = dup_axis())
  scale <- layer_scales(p)$x
  expect_equal(scale$sec_name(), scale$name)
  breaks <- scale$break_info()
  expect_equal(breaks$minor, breaks$sec.minor)
  expect_equal(breaks$major_source, breaks$sec.major_source)
})

test_that("custom breaks work", {
  custom_breaks <- c(0.01, 0.1, 1, 10, 100)
  p <- ggplot(foo, aes(x, y)) +
    geom_point() +
    scale_x_continuous(
      name = "Unit A",
      sec.axis = sec_axis(
        trans = y~.,
        breaks = custom_breaks
      )
    )
  scale <- layer_scales(p)$x
  breaks <- scale$break_info()
  expect_equal(custom_breaks, breaks$sec.major_source)
})

test_that("sec axis works with skewed transform", {
  expect_doppelganger(
    "sec_axis, skewed transform",
    ggplot(foo, aes(x, y)) +
      geom_point() +
      scale_x_continuous(name = "Unit A", trans = 'log',
                         breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                         sec.axis = sec_axis(~. * 100, name = "Unit B",
                                             labels = derive(),
                                             breaks = derive()))
  )
})
