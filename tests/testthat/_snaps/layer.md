# layer() checks its input

    Code
      layer(stat = "identity", position = "identity")
    Condition
      Error:
      ! Can't create layer without a geom.
    Code
      layer(geom = "point", position = "identity")
    Condition
      Error:
      ! Can't create layer without a stat.
    Code
      layer(geom = "point", stat = "identity")
    Condition
      Error:
      ! Can't create layer without a position.
    Code
      layer("point", "identity", mapping = 1:4, position = "identity")
    Condition
      Error:
      ! `mapping` must be created by `aes()`.
    Code
      layer("point", "identity", mapping = ggplot(), position = "identity")
    Condition
      Error:
      ! `mapping` must be created by `aes()`.
      i Did you use `%>%` or `|>` instead of `+`?

---

    Code
      check_subclass("test", "geom")
    Condition
      Error:
      ! Can't find geom called "test".
    Code
      check_subclass(environment(), "geom")
    Condition
      Error in `check_subclass()`:
      ! `x` must be either a string or a <geom> object, not an environment.

# invalid aesthetics throws errors

    Code
      ggplot_build(p)
    Message
      Don't know how to automatically pick scale for object of type <function>. Defaulting to continuous.
    Condition
      Error in `geom_point()`:
      ! Problem while computing aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `compute_aesthetics()`:
      ! Aesthetics are not valid data columns.
      x The following aesthetics are invalid:
      x `fill = data`
      i Did you mistype the name of a data column or forget to add `after_stat()`?

---

    Code
      ggplot_build(p)
    Condition
      Error in `geom_point()`:
      ! Problem while mapping stat to aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `map_statistic()`:
      ! Aesthetics must be valid computed stats.
      x The following aesthetics are invalid:
      x `fill = after_stat(data)`
      i Did you map your stat in the wrong layer?

# missing aesthetics trigger informative error

    Code
      p <- ggplot(df) + geom_line()
      ggplot_build(p)
    Condition
      Error in `geom_line()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `compute_geom_1()`:
      ! `geom_line()` requires the following missing aesthetics: x and y.
    Code
      p <- ggplot(df) + geom_col()
      ggplot_build(p)
    Condition
      Error in `geom_col()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `compute_geom_1()`:
      ! `geom_col()` requires the following missing aesthetics: x and y.

# function aesthetics are wrapped with after_stat()

    Code
      ggplot_build(ggplot(df, aes(colour = density, fill = density)) + geom_point())
    Message
      Don't know how to automatically pick scale for object of type <function>. Defaulting to continuous.
      Don't know how to automatically pick scale for object of type <function>. Defaulting to continuous.
    Condition
      Error in `geom_point()`:
      ! Problem while computing aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `compute_aesthetics()`:
      ! Aesthetics are not valid data columns.
      x The following aesthetics are invalid:
      x `colour = NULL`
      x `fill = NULL`
      i Did you mistype the name of a data column or forget to add `after_stat()`?

# computed stats are in appropriate layer

    Code
      ggplot_build(ggplot(df, aes(colour = after_stat(density), fill = after_stat(
        density))) + geom_point())
    Condition
      Error in `geom_point()`:
      ! Problem while mapping stat to aesthetics.
      i Error occurred in the 1st layer.
      Caused by error in `map_statistic()`:
      ! Aesthetics must be valid computed stats.
      x The following aesthetics are invalid:
      x `colour = NULL`
      x `fill = NULL`
      i Did you map your stat in the wrong layer?

# layer reports the error with correct index etc

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_linerange()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `compute_geom_1()`:
      ! `geom_linerange()` requires the following missing aesthetics: ymax or xmin and xmax.

---

    Code
      ggplotGrob(p)
    Condition
      Error in `geom_boxplot()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 2nd layer.
      Caused by error in `draw_group()`:
      ! Can only draw one boxplot per group.
      i Did you forget `aes(group = ...)`?

# layer warns for constant aesthetics

    Code
      e <- ggplot_build(p)
    Condition
      Warning in `geom_point()`:
      All aesthetics have length 1, but the data has 32 rows.
      i Did you mean to use `annotate()`?

# layer_data returns a data.frame

    Code
      l$layer_data(mtcars)
    Condition
      Error in `layer_data()`:
      ! `layer_data()` must return a <data.frame>.

