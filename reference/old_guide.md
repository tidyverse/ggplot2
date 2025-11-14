# The previous S3 guide system

The guide system has been overhauled to use the ggproto infrastructure
to accommodate guide extensions with the same flexibility as layers,
scales and other ggplot2 objects. In rewriting, the old S3 system has
become defunct, meaning that the previous methods for guides have been
superseded by ggproto methods. As a fallback option, the generics, but
not the methods, that the previous S3 system used are encapsulated in
the `GuideOld` ggproto class.

## Usage

``` r
guide_train(guide, scale, aesthetic = NULL)

guide_merge(guide, new_guide)

guide_geom(guide, layers, default_mapping = NULL)

guide_transform(guide, coord, panel_params)

guide_gengrob(guide, theme)

old_guide(guide)
```

## Arguments

- guide:

  An old guide object
