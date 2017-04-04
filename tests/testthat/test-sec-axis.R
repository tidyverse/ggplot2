context("sec-axis")

x <- exp(seq(log(0.001), log(1000), length.out = 100))
foo <- data.frame(
  x = x,
  y = x/(1+x)
)

test_that("dup_axis() works", {
  vdiffr::expect_doppelganger(
    "dup_axis",
    ggplot(foo, aes(x, y)) +
      geom_point() +
      scale_x_continuous(name = "Unit A",
                         sec.axis = dup_axis())
  )
})

test_that("custom breaks works", {
  vdiffr::expect_doppelganger(
    "sec_axis, custom breaks",
    ggplot(foo, aes(x, y)) +
      geom_point() +
      scale_x_continuous(
        name = "Unit A",
        sec.axis = sec_axis(
          trans = y~.,
          breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
        )
      )
  )
})

test_that("sec axis works with skewed transform", {
  vdiffr::expect_doppelganger(
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
