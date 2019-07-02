
guide_none <- function() {
  structure(list(available_aes = "any"), class = c("guide", "guide_none"))
}

guide_train.guide_none <- function(guide, scale, aesthetic = NULL) {
  guide
}

guide_merge.guide_none <- function(guide, new_guide) {
  guide
}

guide_geom.guide_none <- function(guide, layers, default_mapping) {
  guide
}

guide_gengrob.guide_none <- function(guide, theme, ...) {
  zeroGrob()
}
