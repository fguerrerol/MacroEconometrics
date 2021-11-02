source("PS2_Bootstrap.R")
source("PS2_Miscellaneous.R")

# SVAR Tools ####
companion <- function(Phi, m, p) {
  if (p == 1) {
    matrix(Phi, m, m)
  } else {
    rbind(matrix(Phi, m, m * p), cbind(diag(m * (p - 1)), matrix(0, m * (p - 1), m)))
  }
}

var2vma <- function(Phi, P, m, p, H) {
  F <- companion(Phi, m, p)
  F.temp <- F
  I <- array(NA, c(m, m, H + 1))
  I[, , 1] <- P
  for (h in 1:H) {
    I[, , h + 1] <- F.temp[1:m, 1:m] %*% P
    F.temp <- F.temp %*% F
  }
  I
}

sirf <- function(Phi, P, m, p, H, cumulative = FALSE) {
  I <- var2vma(Phi, P, m, p, H)
  if (isFALSE(cumulative)) {
    I
  } else {
    aperm(apply(I, c(1, 2), cumsum), c(2, 3, 1))
  }
}

fevd <- function(Phi, P, m, p, H) {
  I <- var2vma(Phi, P, m, p, H)
  M <- I[, , 1] ^ 2
  J <- tcrossprod(I[, , 1])
  W <- array(NA, c(m, m, H + 1))
  W[, , 1] <- M / diag(J)
  for (h in 2:(H + 1)) {
    M <- M + I[, , h] ^ 2
    J <- J + tcrossprod(I[, , h])
    W[, , h] <- M / diag(J)
  }
  100 * W
}

hd <- function(Phi, P, Y, E, m, p, T) {
  I <- var2vma(Phi, P, m, p, T - 1)
  U <- t(solve(P, t(E)))
  hist.decomp <- array(NA, c(m, m + 1, T))
  for (t in 1:T) {
    for (j in 1:m) {
      for (i in 1:m) {
        hist.decomp[i, j, t] <- c(crossprod(I[i, j, 1:t], U[t:1, j]))
      }
    }
  }
  non.stochastic <- Y - t(apply(hist.decomp[, 1:m, ], c(1, 3), sum))
  for (i in 1:m) {
    hist.decomp[i, m + 1, ] <- non.stochastic[, i]
  }
  hist.decomp
}

erpt <- function(Phi, P, m, p, H, vx, vy, cumulative = TRUE) {
  if (isTRUE(cumulative)) {
    I <- sirf(Phi, P, m, p, H, cumulative = TRUE)
  } else {
    I <- sirf(Phi, P, m, p, H)
  }
  100 * (I[vx, vy, ] / I[vy, vy, ])
}

# SVAR Tools (vars) ####
SVAR.sirf <- function(SVAR, H, cumulative = FALSE) {
  
  Y <- SVAR$var$y
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- solve(SVAR$A, SVAR$B)
  
  SIRF <- sirf(B$Phi, P, m, p, H, cumulative = cumulative)
  dimnames(SIRF)[[1]] <- toupper(colnames(Y))
  dimnames(SIRF)[[2]] <- paste("S.", 1:m, sep = "")
  dimnames(SIRF)[[3]] <- 0:H
  
  SIRF
  
}

SVAR.fevd <- function(SVAR, H) {
  
  Y <- SVAR$var$y
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- solve(SVAR$A, SVAR$B)
  
  FEVD <- fevd(B$Phi, P, m, p, H)
  dimnames(FEVD)[[1]] <- toupper(colnames(Y))
  dimnames(FEVD)[[2]] <- paste("S.", 1:m, sep = "")
  dimnames(FEVD)[[3]] <- 0:H
  
  FEVD
  
}

SVAR.hd <- function(SVAR) {
  
  Y <- SVAR$var$datamat[, 1:m]
  E <- wash(resid(SVAR$var))
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- solve(SVAR$A, SVAR$B)
  
  HD <- hd(B$Phi, P, Y, E, m, p, T)
  dimnames(HD)[[1]] <- toupper(colnames(Y))
  dimnames(HD)[[2]] <- c(paste("S.", 1:m, sep = ""), "NS")
  dimnames(HD)[[3]] <- 1:T
  
  HD
  
}

SVAR.erpt <- function(SVAR, H, vx, vy, cumulative = TRUE) {

  m <- SVAR$var$K
  p <- SVAR$var$p
  
  B <- VAR.coefficients(SVAR$var)
  P <- solve(SVAR$A, SVAR$B)
  
  erpt(B$Phi, P, m, p, H, vx, vy, cumulative = cumulative)
  
}

# SVAR Tools (Bootstrap) ####
SVAR.sirf.boot <- function(SVAR, Amat, Bmat, H, gamma, Y.boot, cumulative = FALSE) {
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  I.boot <- array(NA, c(m, m, H + 1, R))
  var.names <- colnames(SVAR$var$y)
  
  for (r in 1:dim(Y.boot)[3]) {
    RVAR.boot <- boot.estimate(var.names, Y.boot[, , r], m, p, lag.max, ic, resmat = SVAR$var$restrictions)
    SVAR.boot <- SVAR(RVAR.boot, Amat = Amat, Bmat = Bmat, lrtest = FALSE, max.iter = 500)
    I.boot[, , , r] <- SVAR.sirf(SVAR.boot, H, cumulative)
  }
  
  I.boot <- aperm(I.boot, c(4, 1, 2, 3))
  
  Ic <- SVAR.sirf(SVAR, H, cumulative)
  Il <- array(NA, dim(Ic))
  Iu <- array(NA, dim(Ic))
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        ci <- boot.ci(I.boot[, i, j, h], gamma)
        Il[i, j, h] <- ci[1]
        Iu[i, j, h] <- ci[2]
      }
    }
  }
  dimnames(Il) <- dimnames(Ic)
  dimnames(Iu) <- dimnames(Ic)
  
  list(lb = Il, pe = Ic, ub = Iu)
  
}

SVAR.fevd.boot <- function(SVAR, Amat, Bmat, H, gamma, Y.boot) {
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  F.boot <- array(NA, c(m, m, H + 1, R))
  var.names <- colnames(SVAR$var$y)
  
  for (r in 1:dim(Y.boot)[3]) {
    RVAR.boot <- boot.estimate(var.names, Y.boot[, , r], m, p, lag.max, ic, resmat = SVAR$var$restrictions)
    SVAR.boot <- SVAR(RVAR.boot, Amat = Amat, Bmat = Bmat, lrtest = FALSE, max.iter = 500)
    F.boot[, , , r] <- SVAR.fevd(SVAR.boot, H)
  }
  
  F.boot <- aperm(F.boot, c(4, 1, 2, 3))
  
  Fc <- SVAR.fevd(SVAR, H)
  Fl <- array(NA, dim(Fc))
  Fu <- array(NA, dim(Fc))
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        ci <- boot.ci(F.boot[, i, j, h], gamma)
        Fl[i, j, h] <- ci[1]
        Fu[i, j, h] <- ci[2]
      }
    }
  }
  dimnames(Fl) <- dimnames(Fc)
  dimnames(Fu) <- dimnames(Fc)
  
  list(lb = Fl, pe = Fc, ub = Fu)
  
}

SVAR.erpt.boot <- function(SVAR, Amat, Bmat, H, vx, vy, gamma, Y.boot, cumulative = TRUE) {
  
  m <- SVAR$var$K
  p <- SVAR$var$p
  
  E.boot <- matrix(NA, H + 1, R)
  var.names <- colnames(SVAR$var$y)
  
  for (r in 1:dim(Y.boot)[3]) {
    RVAR.boot <- boot.estimate(var.names, Y.boot[, , r], m, p, lag.max, ic, resmat = SVAR$var$restrictions)
    SVAR.boot <- SVAR(RVAR.boot, Amat = Amat, Bmat = Bmat, lrtest = FALSE, max.iter = 500)
    E.boot[, r] <- SVAR.erpt(SVAR.boot, H, vx, vy, cumulative)
  }
  
  E.boot <- aperm(E.boot, c(2, 1))
  
  Ec <- SVAR.erpt(SVAR, H, vx, vy, cumulative)
  El <- rep(NA, H + 1)
  Eu <- rep(NA, H + 1)
  for (h in 1:(H + 1)) {
    ci <- boot.ci(E.boot[, h], gamma)
    El[h] <- ci[1]
    Eu[h] <- ci[2]
  }
  
  list(lb = El, pe = Ec, ub = Eu)
  
}