Trans <- proto(TopLevel, {
  .transform <- force
  .transform_inverse <- force
  .transform_labels <- transform 

  objname <- "Transformer"
  class <- function(.) "trans"

  new <- function(., name, f="force", inverse="force", labels="force", ...) {
    .$proto(
      objname = name, 
      .transform = f,
      .transform_inverse = inverse,
      .transform_labels = labels,
      ...
    )
  }
  
  transform <- function(., values) {
    if (is.null(values)) return()
    match.fun(get(".transform", .))(values)
  }

  inverse <- function(., values) {
    if (is.null(values)) return()
    match.fun(get(".transform_inverse", .))(values)
  }

  label <- function(., values) {
    if (is.null(values)) return()
    lapply(values, match.fun(get(".transform_labels", .)))
  }

  # Create regular sequence in original scale, then transform back
  seq <- function(., from, to, length) {
    .$transform(get("seq", pos=1)(.$inverse(from), .$inverse(to), length=length))
  }
  
  input_breaks <- function(., range) {
    grid.pretty(range)
  }
  
  # Minor breaks are regular on the original scale
  # and need to cover entire range of plot
  output_breaks <- function(., n = 2, b, r) {
    if (length(b) == 1) return(b)

    bd <- diff(b)[1]
    if (min(r) < min(b)) b <- c(b[1] - bd, b)
    if (max(r) > max(b)) b <- c(b, b[length(b)] + bd)
    unique(unlist(mapply(.$seq, b[-length(b)], b[-1], length=n+1, SIMPLIFY=F)))
  }
  
  
  check <- function(., values) {
    .$inverse(.$transform(values))
  }

  pprint <- function(., newline=TRUE) {
    cat(deparse(get(".transform", .)), " <-> ", deparse(get(".transform_inverse", .)))
    if (newline) cat("\n") 
  }
  
})


PowerTrans <- proto(Trans, {
  new <- function(., exponent) {
    .$proto(name = paste("pow", exponent, sep=""), p = exponent)
  }
  transform <- function(., values) {
    (values^.$p - 1) / .$p * sign(values - 1)
  }
  inverse <- function(., values) {
    (abs(values) * .$p + 1 * sign(values)) ^ (1 / .$p) 
  }
  label <- function(., values) .$inverse(values)
})

ProbabilityTrans <- proto(Trans, {
  new <- function(., family) {
    .$proto(name=family, family = family)
  }
  transform <- function(., values) {
    if (is.null(values)) return()
    match.fun(paste("q", .$family, sep=""))(values)
  }
  inverse <- function(., values) {
    match.fun(paste("p", .$family, sep=""))(values)
  }
  label <- function(., values) .$inverse(values)
})

TransAsn <- Trans$new(
  "asn", 
  function(x) 2 * asin(sqrt(x)), 
  function(x) sin(x / 2)^2
)

TransAtanh <- Trans$new(
  "atanh", 
  "tanh", 
  "force"
)
  
TransExp <- Trans$new("exp", "exp", "log", function(x) bquote(log(.(x))))
TransIdentity <- Trans$new("identity", "force", "force", "force")
TransInverse <- Trans$new("inverse", function(x) 1/x, function(x) 1/x,  function(x) bquote(phantom()^1 / phantom()[.(x)]))
TransLog <- Trans$new("log", "log", "exp", function(x) bquote(e^.(x)))
TransLog10 <- Trans$new("log10", "log10", function(x) 10^x, function(x) bquote(10^.(x)))
TransLog2 <- Trans$new("log2", "log2", function(x) 2^x, function(x) bquote(2^.(x)))
TransLogit <- ProbabilityTrans$new("logis")
TransPow10 <- Trans$new("pow10",function(x) 10^x, "log10", function(x) log10(x))
TransProbit <- ProbabilityTrans$new("norm")
TransReverse <- Trans$new("reverse", function(x) -x, function(x) -x, function(x) bquote(.(-x)))
TransSqrt <- Trans$new("sqrt", "sqrt", function(x) x^2, function(x) x^2)

TransDate <- Trans$new("date", "as.numeric", "to_date")
TransDatetime <- Trans$new("datetime", "as.numeric", "to_time")
