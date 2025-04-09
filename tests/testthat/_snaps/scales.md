# oob affects position values

    Removed 1 row containing missing values or values outside the scale range (`geom_bar()`).

---

    Removed 3 rows containing missing values or values outside the scale range (`geom_bar()`).

# scales warn when transforms introduces non-finite values

    log-10 transformation introduced infinite values.

# size and alpha scales throw appropriate warnings for factors

    Using size for a discrete variable is not advised.

---

    Using alpha for a discrete variable is not advised.

---

    Using linewidth for a discrete variable is not advised.

# shape scale throws appropriate warnings for factors

    Using shapes for an ordinal variable is not advised

# scale_apply preserves class and attributes

    `scale_id` must not contain any "NA".

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

# training incorrectly appropriately communicates the offenders

    Continuous values supplied to discrete scale.
    i Example values: 1, 2, 3, 4, and 5

---

    Discrete values supplied to continuous scale.
    i Example values: "A", "B", "C", "D", and "E"

# Using `scale_name` prompts deprecation message

    The `scale_name` argument of `continuous_scale()` is deprecated as of ggplot2 3.5.0.

---

    The `scale_name` argument of `discrete_scale()` is deprecated as of ggplot2 3.5.0.

---

    The `scale_name` argument of `binned_scale()` is deprecated as of ggplot2 3.5.0.

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

