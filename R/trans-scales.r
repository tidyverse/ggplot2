ScaleArea <- proto(
  ScaleSizeContinuous, 
  desc = "Scale area",
  objname = "area", 
  icon = function(.) ScaleSize$icon(), 
  details = "", 
  doc = FALSE,
  new = function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, to = c(1, 6)) {
    .super$new(., name = name, limits = limits, breaks = breaks, 
       labels = labels, trans="sqrt", to = to)
  }
  
)

ScaleSqrt <-     proto(ScaleContinuous, 
  desc = "Position scale, square root transformed",
  tr_default = Trans$find("sqrt"),     
  objname = "sqrt", 
  doc=FALSE, 
  examples=function(.) {}
)

ScaleLog10 <-    proto(ScaleContinuous,
  desc = "Position scale, log10 transformed",
  tr_default = Trans$find("log10"),
  objname = "log10",
  doc=FALSE,
  examples=function(.) {}
)

ScalePow10 <-    proto(ScaleContinuous,
  desc = "Position scale, pow10 transformed",
  tr_default = Trans$find("pow10"),
  objname = "pow10",
  doc=FALSE,
  examples=function(.) {}
)

ScaleLog2 <-     proto(ScaleContinuous,
  desc = "Position scale, log2 transformed",
  tr_default = Trans$find("log2"),
  objname = "log2",
  doc=FALSE,
  examples=function(.) {}
)

ScaleLog <-      proto(ScaleContinuous,
  desc = "Position scale, log transformed",
  tr_default = Trans$find("log"),
  objname = "log",
  doc=FALSE,
  examples=function(.) {}
)

ScaleExp <-      proto(ScaleContinuous,
  desc = "Position scale, exponential transformed",
  tr_default = Trans$find("exp"),
  objname = "exp",
  doc=FALSE,
  examples=function(.) {}
)

ScaleLogit <-    proto(ScaleContinuous,
  desc = "Position scale, logit transformed",
  tr_default = Trans$find("logit"),
  objname = "logit",
  doc=FALSE,
  examples=function(.) {}
)

ScaleReverse <-    proto(ScaleContinuous,
  desc = "Position scale, axis direction reversed",
  tr_default = Trans$find("reverse"),
  objname = "reverse",
  doc=FALSE,
  examples=function(.) {}
)

ScaleAsn <-      proto(ScaleContinuous,
  desc = "Position scale, arc-sin transformed",
  tr_default = Trans$find("asn"),
  objname = "asn",
  doc=FALSE,
  examples=function(.) {}
)

ScaleProbit <-   proto(ScaleContinuous,
  desc = "Position scale, probit transformed",
  tr_default = Trans$find("probit"),
  objname = "probit",
  doc=FALSE,
  examples=function(.) {}
)

ScaleAtanh <-    proto(ScaleContinuous,
  desc = "Position scale, arc-hyperbolic tangent transformed",
  tr_default = Trans$find("atanh"),
  objname = "atanh",
  doc=FALSE,
  examples=function(.) {}
)

ScaleInverse <-  proto(ScaleContinuous,
  desc = "Position scale, inverse transformed",
  tr_default = Trans$find("inverse"),
  objname = "inverse",
  doc=FALSE,
  examples=function(.) {}
)
ScaleRecip <-  proto(ScaleContinuous,
  desc = "Position scale, reciprocal",
  tr_default = Trans$find("inverse"),
  objname = "recip",
  doc=FALSE,
  examples=function(.) {}
)

ScaleContinuous$tr_default <- Trans$find("identity")

ScaleProb <- proto(ScaleContinuous, {
  doc <- FALSE
  objname <- "prob"
  desc <- "Probability scale"
  icon <- function(.) {
    textGrob("P()", gp=gpar(cex=1.5))
  }
  new <- function(., name=NULL, limits=c(NA,NA), breaks=NULL, family="norm", variable="x") {
    .$proto(name=name, .input=variable, .output=variable, limits=limits, breaks = breaks, .tr = ProbabilityTrans$new(family), family=family)
  }
  examples <- function(.) {
    # Coming soon
  }
#  output_set <- function(.) c(0, 1)
})

ScalePow <- proto(ScaleContinuous, {
  doc <- FALSE
  objname <- "pow"
  desc <- "Power scale"
  icon <- function(.) {
    textGrob(expression(frac(x ^ (alpha - 1), alpha)), gp=gpar(cex=1.2))
  }
  new <- function(., name=NULL, limits=c(NA,NA), breaks=NULL, power=1, variable) {
    .$proto(name=name, .input=variable, .output=variable, limits=limits, breaks = breaks, .tr = PowerTrans$new(power), power=power)
  }
  examples <- function(.) {
    # Coming soon
  }
})