# check_required_aesthetics() errors on missing

    Code
      check_required_aesthetics(required_single, present = "x", name = "test")
    Condition
      Error:
      ! `test()` requires the following missing aesthetics: y.

---

    Code
      check_required_aesthetics(required_single, present = "shape", name = "test")
    Condition
      Error:
      ! `test()` requires the following missing aesthetics: x and y.

---

    Code
      check_required_aesthetics(required_bidirectional, present = "fill", name = "test")
    Condition
      Error:
      ! `test()` requires the following missing aesthetics: x or y.

---

    Code
      check_required_aesthetics(required_bidirectional, present = "shape", name = "test")
    Condition
      Error:
      ! `test()` requires the following missing aesthetics: x and fill or y and fill.

# remove_missing checks input

    Code
      remove_missing(na.rm = 1:5)
    Condition
      Error in `remove_missing()`:
      ! `na.rm` must be `TRUE` or `FALSE`, not an integer vector.

# tolower() and toupper() has been masked

    Code
      tolower()
    Condition
      Error in `tolower()`:
      ! Please use `to_lower_ascii()`, which works fine in all locales.

---

    Code
      toupper()
    Condition
      Error in `toupper()`:
      ! Please use `to_upper_ascii()`, which works fine in all locales.

# parse_safe() checks input

    Code
      parse_safe(1:5)
    Condition
      Error in `parse_safe()`:
      ! `text` must be a character vector, not an integer vector.

# width_cm() and height_cm() checks input

    Code
      width_cm(letters)
    Condition
      Error in `width_cm()`:
      ! Don't know how to get width of <character> object

---

    Code
      height_cm(letters)
    Condition
      Error in `height_cm()`:
      ! Don't know how to get height of <character> object

# cut_*() checks its input and output

    Code
      cut_number(1, 10)
    Condition
      Error in `cut_number()`:
      ! Insufficient data values to produce 10 bins.

---

    Code
      breaks(1:10, "numbers", nbins = 2, binwidth = 5)
    Condition
      Error in `breaks()`:
      ! Specify exactly one of `n` and `width`.

---

    Code
      cut_width(1:10, 1, center = 0, boundary = 0.5)
    Condition
      Error in `cut_width()`:
      ! Only one of `boundary` and `center` may be specified.

# interleave() checks the vector lengths

    Code
      interleave(1:4, numeric())
    Condition
      Error in `vec_interleave()`:
      ! Can't recycle `..1` (size 4) to match `..2` (size 0).

