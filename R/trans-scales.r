ScaleArea <- proto(ScaleSize, .tr = Trans$find("sqrt"), objname="area", icon=function(.) ScaleSize$icon(), details = "", doc = FALSE)

ScaleSqrt <-     proto(ScaleContinuous, tr_default = Trans$find("sqrt"),     objname = "sqrt", doc=FALSE, examples=function(.) {})
ScaleLog10 <-    proto(ScaleContinuous, tr_default = Trans$find("log10"),    objname = "log10", doc=FALSE, examples=function(.) {})
ScalePow10 <-    proto(ScaleContinuous, tr_default = Trans$find("pow10"),    objname = "pow10", doc=FALSE, examples=function(.) {})
ScaleLog2 <-     proto(ScaleContinuous, tr_default = Trans$find("log2"),     objname = "log2", doc=FALSE, examples=function(.) {})
ScaleLog <-      proto(ScaleContinuous, tr_default = Trans$find("log"),      objname = "log", doc=FALSE, examples=function(.) {})
ScaleExp <-      proto(ScaleContinuous, tr_default = Trans$find("exp"),      objname = "exp", doc=FALSE, examples=function(.) {})
ScaleLogit <-    proto(ScaleContinuous, tr_default = Trans$find("logit"),    objname = "logit", doc=FALSE, examples=function(.) {})
ScaleReverse <-    proto(ScaleContinuous, tr_default = Trans$find("reverse"),    objname = "reverse", doc=FALSE, examples=function(.) {})
ScaleAsn <-      proto(ScaleContinuous, tr_default = Trans$find("asn"),      objname = "asn", doc=FALSE, examples=function(.) {})
ScaleProbit <-   proto(ScaleContinuous, tr_default = Trans$find("probit"),   objname = "probit", doc=FALSE, examples=function(.) {})
ScaleAtanh <-    proto(ScaleContinuous, tr_default = Trans$find("atanh"),    objname = "atanh", doc=FALSE, examples=function(.) {})
ScaleInverse <-  proto(ScaleContinuous, tr_default = Trans$find("inverse"),  objname = "inverse", doc=FALSE, examples=function(.) {})
ScaleContinuous$tr_default <- Trans$find("identity")

ScaleProb <- proto(ScaleContinuous, {
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
#  frange <- function(.) c(0, 1)
})

ScalePow <- proto(ScaleContinuous, {
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