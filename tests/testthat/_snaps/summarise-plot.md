# summarise_*() throws appropriate errors

    Code
      summarise_layout(10)
    Condition
      Error in `summarise_layout()`:
      ! `p` must be a <ggplot_built> object, not the number 10.

---

    Code
      summarise_coord("A")
    Condition
      Error in `summarise_coord()`:
      ! `p` must be a <ggplot_built> object, not the string "A".

---

    Code
      summarise_layers(TRUE)
    Condition
      Error in `summarise_layers()`:
      ! `p` must be a <ggplot_built> object, not `TRUE`.

