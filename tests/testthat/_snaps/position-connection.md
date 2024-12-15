# position_connection validates connections

    Code
      p$setup_params(NULL)
    Condition
      Error in `position_connect()`:
      ! `connection` must be one of "hv", "vh", "mid", or "linear", not "foobar".

---

    Code
      p$setup_params(NULL)
    Condition
      Error in `position_connect()`:
      ! `connection` must be a numeric <matrix> with 2 columns, not an integer matrix with 1 columns.

---

    Code
      p$setup_params(NULL)
    Condition
      Error in `position_connect()`:
      ! `connection` cannot contain missing or other non-finite values.

