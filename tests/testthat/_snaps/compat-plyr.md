# input checks work in compat functions

    Code
      unrowname(1:6)
    Condition
      Error in `unrowname()`:
      ! Can only remove rownames from <data.frame> and <matrix> objects.

---

    Code
      revalue(1:7, c(`5` = 2))
    Condition
      Error in `revalue()`:
      ! `x` must be a factor or character vector, not an integer vector.

---

    Code
      as.quoted(1:7)
    Condition
      Error in `as.quoted()`:
      ! Must be a character vector, call, or formula.

---

    Code
      round_any(letters)
    Condition
      Error in `round_any()`:
      ! `x` must be a <numeric> vector, not a character vector.

