wash <- function(X) {
  unclass(as.matrix(unname(X)))[]
}

VAR.coefficients <- function(VAR) {
  B <- wash(Bcoef(VAR))
  list(Pi = B[, m * p + 1], Phi = B[, 1:(m * p)])
}