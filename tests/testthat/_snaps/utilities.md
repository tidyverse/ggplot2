# parse_safe() checks input

    `text` must be a character vector, not an integer vector.

# check_required_aesthetics() errors on missing

    `test()` requires the following missing aesthetics: y.

---

    `test()` requires the following missing aesthetics: x and y.

---

    `test()` requires the following missing aesthetics: x or y.

---

    `test()` requires the following missing aesthetics: fill and x or y.

# remove_missing checks input

    `na.rm` must be `TRUE` or `FALSE`, not an integer vector.

# characters survive remove_missing

    Removed 1 row containing non-finite outside the scale range.

# tolower() and toupper() has been masked

    Please use `to_lower_ascii()`, which works fine in all locales.

---

    Please use `to_upper_ascii()`, which works fine in all locales.

