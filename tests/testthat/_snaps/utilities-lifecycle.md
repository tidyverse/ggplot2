# editions can be set and unset

    Code
      set_edition("nonsense")
    Condition
      Error in `set_edition()`:
      ! `edition` must be one of "2025" or "2026", not "nonsense".

# edition deprecation works

    `foo()` was deprecated in ggplot2 4.0.0.
    i Please use `bar()` instead.

---

    Code
      foo()
    Condition
      Error:
      ! `foo()` was deprecated in ggplot2 4.0.0 and is now defunct.
      i Please use `bar()` instead.

# edition supersession works

    Code
      foo()
    Condition
      Error:
      ! `foo()` was deprecated in <NA> edition 2025 and is now defunct.
      i Please use `bar()` instead.

# edition requirements work

    Code
      foo()
    Condition
      Error in `foo()`:
      ! foo() requires the 2025 edition of ggplot2.

