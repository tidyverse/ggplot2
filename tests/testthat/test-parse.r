context("Parsing expressions")

expect_equal(
  parse_safe(c("", " ", " \n  \n ")),
  expression(NA, NA, NA)
)

expect_equal(
  parse_safe(c("A", "B", "C")),
  expression(A, B, C)
)

expect_equal(
  parse_safe(c("alpha", "beta", "gamma")),
  expression(alpha, beta, gamma)
)

expect_equal(
  parse_safe(c("alpha", "", "gamma", " ")),
  expression(alpha, NA, gamma, NA)
)

expect_equal(
  parse_safe(c("alpha ~ beta", " ", "integral(f(x) * dx, a, b)")),
  expression(alpha ~ beta, NA, integral(f(x) * dx, a, b))
)

expect_equal(
  parse_safe(c("alpha \n beta", " ", "integral(f(x) * dx, a, b)")),
  expression(alpha, NA, integral(f(x) * dx, a, b))
)

expect_equal(
  parse_safe(c(NA, "a", NA, "alpha")),
  expression(NA, a, NA, alpha)
)

expect_equal(
  parse_safe(factor(c("alpha", "beta", ""))),
  expression(2, 3, 1)
)

expect_equal(
  parse_safe(factor(c(NA, "a", NA, "alpha"))),
  expression(NA, 1, NA, 2)
)

expect_equal(
  parse_safe(c(NA, 1, 2, "a \n b")),
  expression(NA, 1, 2, a)
)

