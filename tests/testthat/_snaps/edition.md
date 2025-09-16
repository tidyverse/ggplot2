# basic edition infrastructure works as intended

    Code
      edition_deprecate(2025, what = "foo()")
    Condition
      Error:
      ! `foo()` was deprecated in ggplot2 edition 2025 and is now defunct.

---

    Code
      edition_require(2025, what = "foo()")
    Condition
      Error in `edition_require()`:
      ! foo() requires the 2025 edition of ggplot2.

