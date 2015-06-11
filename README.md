# ggplot2

The only difference between this repos and hadley/ggplot2 is that in
this repos, you can use non-standard theme options via

```r
theme(animint.width=800, validate=FALSE)
```

in animint we actually would write

```r
theme_animint(width=800)
```
