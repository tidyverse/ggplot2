# stat_connect rejects invalid connections

    Code
      test_setup(connection = "foobar")
    Condition
      Error in `setup_params()`:
      ! `connection` must be one of "hv", "vh", "mid", or "linear", not "foobar".

---

    Code
      test_setup(connection = matrix(1:3, ncol = 1))
    Condition
      Error in `setup_params()`:
      ! `connection` must be a numeric <matrix> with 2 columns, not an integer matrix with 1 column(s).

---

    Code
      test_setup(connection = matrix(c(1:3, NA), ncol = 2))
    Condition
      Error in `setup_params()`:
      ! `connection` cannot contain missing or other non-finite values.

