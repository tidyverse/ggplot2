# training incorrectly appropriately communicates the offenders

    Continuous value supplied to a discrete scale.
    i Example values: 1, 2, 3, 4, and 5.

---

    Discrete value supplied to a continuous scale.
    i Example values: "A" and "E".

# Using `scale_name` prompts deprecation message

    The `scale_name` argument of `continuous_scale()` is deprecated as of ggplot2 3.5.0.

---

    The `scale_name` argument of `discrete_scale()` is deprecated as of ggplot2 3.5.0.

---

    The `scale_name` argument of `binned_scale()` is deprecated as of ggplot2 3.5.0.

# mismatch between `na.value` and `palette` throws error

    Code
      sc$map(x)
    Condition
      Error in `vec_assign()`:
      ! Can't convert `na.value` <double> to match type of `palette` <character>.

# continuous scales warn about faulty `limits`

    Code
      scale_x_continuous(limits = c("A", "B"))
    Condition
      Error in `scale_x_continuous()`:
      ! `limits` must be a <numeric> vector, not a character vector.

---

    Code
      scale_x_continuous(limits = 1:3)
    Condition
      Error in `scale_x_continuous()`:
      ! `limits` must be a vector of length 2, not length 3.

# breaks and labels are correctly checked

    `breaks` and `labels` must have the same length.

---

    Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Invalid `minor_breaks` specification. Use `NULL`, not `NA`.

---

    Invalid `labels` specification. Use `NULL`, not `NA`.

---

    `breaks` and `labels` have different lengths.

---

    Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Invalid `labels` specification. Use `NULL`, not `NA`.

---

    `breaks` and `labels` have different lengths.

# labels match breaks

    Code
      scale_x_discrete(breaks = 1:3, labels = 1:2)
    Condition
      Error in `scale_x_discrete()`:
      ! `breaks` and `labels` must have the same length.

---

    Code
      scale_x_continuous(breaks = 1:3, labels = 1:2)
    Condition
      Error in `scale_x_continuous()`:
      ! `breaks` and `labels` must have the same length.

# passing continuous limits to a discrete scale generates a warning

    Continuous limits supplied to discrete scale.
    i Did you mean `limits = factor(...)` or `scale_*_continuous()`?

# suppressing breaks, minor_breask, and labels works

    Code
      scale_x_date(breaks = NA, limits = lims)$get_breaks()
    Condition
      Error in `scale_x_date()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_date(labels = NA, limits = lims)$get_labels()
    Condition
      Error in `scale_x_date()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_date(minor_breaks = NA, limits = lims)$get_breaks_minor()
    Condition
      Error in `scale_x_date()`:
      ! Invalid `minor_breaks` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_datetime(breaks = NA, limits = lims)$get_breaks()
    Condition
      Error in `scale_x_datetime()`:
      ! Invalid `breaks` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_datetime(labels = NA, limits = lims)$get_labels()
    Condition
      Error in `scale_x_datetime()`:
      ! Invalid `labels` specification. Use `NULL`, not `NA`.

---

    Code
      scale_x_datetime(minor_breaks = NA, limits = lims)$get_breaks_minor()
    Condition
      Error in `scale_x_datetime()`:
      ! Invalid `minor_breaks` specification. Use `NULL`, not `NA`.

# numeric scale transforms can produce breaks

    Code
      test_breaks("asn", limits = c(0, 1))
    Output
      [1] 0.00 0.25 0.50 0.75 1.00

---

    Code
      test_breaks("sqrt", limits = c(0, 10))
    Output
      [1]  0.0  2.5  5.0  7.5 10.0

---

    Code
      test_breaks("atanh", limits = c(-0.9, 0.9))
    Output
      [1]   NA -0.5  0.0  0.5   NA

---

    Code
      test_breaks(transform_boxcox(0), limits = c(1, 10))
    Output
      [1]   NA  2.5  5.0  7.5 10.0

---

    Code
      test_breaks(transform_modulus(0), c(-10, 10))
    Output
      [1] -10  -5   0   5  10

---

    Code
      test_breaks(transform_yj(0), c(-10, 10))
    Output
      [1] -10  -5   0   5  10

---

    Code
      test_breaks("exp", c(-10, 10))
    Output
      [1] -10  -5   0   5  10

---

    Code
      test_breaks("identity", limits = c(-10, 10))
    Output
      [1] -10  -5   0   5  10

---

    Code
      test_breaks("log", limits = c(0.1, 1000))
    Output
      [1]        NA   1.00000  20.08554 403.42879

---

    Code
      test_breaks("log10", limits = c(0.1, 1000))
    Output
      [1] 1e-01 1e+00 1e+01 1e+02 1e+03

---

    Code
      test_breaks("log2", limits = c(0.5, 32))
    Output
      [1]  0.5  2.0  8.0 32.0

---

    Code
      test_breaks("log1p", limits = c(0, 10))
    Output
      [1]  0.0  2.5  5.0  7.5 10.0

---

    Code
      test_breaks("pseudo_log", limits = c(-10, 10))
    Output
      [1] -10  -5   0   5  10

---

    Code
      test_breaks("logit", limits = c(0.001, 0.999))
    Output
      [1]   NA 0.25 0.50 0.75   NA

---

    Code
      test_breaks("probit", limits = c(0.001, 0.999))
    Output
      [1]   NA 0.25 0.50 0.75   NA

---

    Code
      test_breaks("reciprocal", limits = c(1, 10))
    Output
      [1]   NA  2.5  5.0  7.5 10.0

---

    Code
      test_breaks("reverse", limits = c(-10, 10))
    Output
      [1] -10  -5   0   5  10

---

    Code
      test_breaks("sqrt", limits = c(0, 10))
    Output
      [1]  0.0  2.5  5.0  7.5 10.0

