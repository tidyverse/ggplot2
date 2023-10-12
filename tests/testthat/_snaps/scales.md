# scale_apply preserves class and attributes

    `scale_id` must not contain any "NA"

# breaks and labels are correctly checked

    `breaks` and `labels` must have the same length

---

    Invalid `breaks` specification. Use "NULL", not "NA".

---

    Invalid `minor_breaks` specification. Use "NULL", not "NA".

---

    Invalid `labels` specification. Use "NULL", not "NA".

---

    `breaks` and `labels` are different lengths.

---

    Invalid `breaks` specification. Use "NULL", not "NA".

---

    Invalid `labels` specification. Use "NULL", not "NA".

---

    Invalid `breaks` specification. Use "NULL", not "NA".

---

    Invalid `labels` specification. Use "NULL", not "NA".

---

    `breaks` and `labels` are different lengths.

# numeric scale transforms can produce breaks

    Code
      test_breaks("log", limits = c(0.1, 1000))
    Output
      [1]        NA   1.00000  20.08554 403.42879

# training incorrectly appropriately communicates the offenders

    Continuous values supplied to discrete scale
    i Example values: 1, 2, 3, 4, and 5

---

    Discrete values supplied to continuous scale
    i Example values: "A", "B", "C", "D", and "E"

# Using `scale_name` prompts deprecation message

    The `scale_name` argument of `continuous_scale()` is deprecated as of ggplot2 3.5.0.

---

    The `scale_name` argument of `discrete_scale()` is deprecated as of ggplot2 3.5.0.

---

    The `scale_name` argument of `binned_scale()` is deprecated as of ggplot2 3.5.0.

