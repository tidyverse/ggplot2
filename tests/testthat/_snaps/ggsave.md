# ggsave can create directories

    Code
      ggsave(path, p)
    Condition
      Error in `ggsave()`:
      ! Cannot find directory 'PATH'
      i Please supply an existing directory or use `create.dir = TRUE`.

# ggsave warns about empty or multiple filenames

    Code
      x <- suppressMessages(ggsave(c(file1, file2), plot))
    Condition
      Warning in `ggsave()`:
      `filename` must have length 1, not 2.
      ! Only the first,'PATH', will be used.

---

    Code
      ggsave(character(), plot)
    Condition
      Error in `ggsave()`:
      ! `filename` must be a single string, not an empty character vector.

# ggsave fails informatively for no-extension filenames

    Code
      ggsave(tempfile(), plot)
    Condition
      Error in `ggsave()`:
      ! Can't save to PATH
      i Either supply `filename` with a file extension or supply `device`.

# warned about large plot unless limitsize = FALSE

    Code
      plot_dim(c(50, 50))
    Condition
      Error:
      ! Dimensions exceed 50 inches (`height` and `width` are specified in inches not pixels).
      i If you're sure you want a plot that big, use `limitsize = FALSE`.

---

    Code
      plot_dim(c(15000, 15000), units = "px")
    Condition
      Error:
      ! Dimensions exceed 50 inches (`height` and `width` are specified in pixels).
      i If you're sure you want a plot that big, use `limitsize = FALSE`.

# unknown device triggers error

    `device` must be a string, function or `NULL`, not the number 1.

---

    Code
      validate_device("xyz")
    Condition
      Error:
      ! Unknown graphics device "xyz"

---

    Code
      validate_device(NULL, "test.xyz")
    Condition
      Error:
      ! Unknown graphics device "xyz"

# invalid single-string DPI values throw an error

    `dpi` must be one of "screen", "print", or "retina", not "abc".

# invalid non-single-string DPI values throw an error

    `dpi` must be a single number or string, not a <factor> object.

---

    `dpi` must be a single number or string, not a character vector.

---

    `dpi` must be a single number or string, not a double vector.

---

    `dpi` must be a single number or string, not a list.

