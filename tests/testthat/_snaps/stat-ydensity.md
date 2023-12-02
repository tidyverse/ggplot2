# calc_bw() requires at least two values and correct method

    Code
      calc_bw(1, "nrd0")
    Condition
      Error in `calc_bw()`:
      ! `x` must contain at least 2 elements to select a bandwidth automatically.

---

    Code
      calc_bw(1:5, "test")
    Condition
      Error in `calc_bw()`:
      ! `test` is not a valid bandwidth rule.

