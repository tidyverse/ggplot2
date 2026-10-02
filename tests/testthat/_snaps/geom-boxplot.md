# geom_boxplot for continuous x gives warning if more than one x (#992)

    Code
      p <- bplot(aes(x, y))
    Condition
      Warning:
      Orientation is not uniquely specified when both the x and y aesthetics are continuous. Picking default orientation 'x'.
      Warning:
      Continuous x aesthetic
      i did you forget `aes(group = ...)`?

---

    Code
      p <- bplot(aes(x, y), facet_wrap(~x))
    Condition
      Warning:
      Orientation is not uniquely specified when both the x and y aesthetics are continuous. Picking default orientation 'x'.
      Warning:
      Continuous x aesthetic
      i did you forget `aes(group = ...)`?

---

    Code
      p <- bplot(aes(Sys.Date() + x, y))
    Condition
      Warning:
      Orientation is not uniquely specified when both the x and y aesthetics are continuous. Picking default orientation 'x'.
      Warning:
      Continuous x aesthetic
      i did you forget `aes(group = ...)`?

# boxplots with a group size >1 error

    Can only draw one boxplot per group.
    i Did you forget `aes(group = ...)`?

