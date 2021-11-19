AR.coefficients <- function(B, m, p) {
  return(t(B[2:((m * p) + 1), ]))
}

AR.companion <- function(A, m, p, r) {
  if (p == 1) {
    return(A)
  } else if (p > 1) {
    return(rbind(A, cbind(diag(m * (p - 1)), matrix(0, nrow = m * (p - 1), ncol = m))))
  }
}

ci.efron <- function(x, a) {
  lb <- (1 - a) / 2
  ub <- 1 - (1 - a) / 2
  ci <- quantile(x,  probs = c(lb, 0.5, ub))
  return(ci)
}

BVAR.sirf <- function(BVAR, H, a, cumulative = FALSE) {
  Ib <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1, BVAR$meta$n_save))
  nvars <- BVAR$variables
  for (r in 1:BVAR$meta$n_save) {
    A.draw <- BVAR$beta[r, , ]
    A.draw <- AR.coefficients(A.draw, BVAR$meta$M, BVAR$meta$lags)
    A.draw <- AR.companion(A.draw, BVAR$meta$M, BVAR$meta$lags, r)
    P.draw <- t(chol(BVAR$sigma[r, , ]))
    if (cumulative == FALSE) {
      Ib[, , , r] <- sirf(A.draw, P.draw, BVAR$meta$M, H, cumulative = FALSE)
    } else {
      Ib[, , , r] <- sirf(A.draw, P.draw, BVAR$meta$M, H, cumulative = TRUE)
    }
  }
  Il <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1))
  Ic <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1))
  Iu <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1))
  for (h in 1:(H + 1)) {
    for (j in 1:BVAR$meta$M) {
      for (i in 1:BVAR$meta$M) {
        ci <- ci.efron(Ib[i, j, h, ], a)
        Il[i, j, h] <- ci[1]
        Ic[i, j, h] <- ci[2]
        Iu[i, j, h] <- ci[3]
      }
    }
  }
  dimnames(Ic)[[1]] <- toupper(nvars)
  dimnames(Ic)[[2]] <- paste("S.", 1:BVAR$meta$M, sep = "")
  dimnames(Ic)[[3]] <- 0:H
  dimnames(Il) <- dimnames(Ic)
  dimnames(Iu) <- dimnames(Ic)
  SIRF <- list(lb = Il, pe = Ic, ub = Iu)
  return(SIRF)
}

BVAR.fevd <- function(BVAR, H, a) {
  Fb <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1, BVAR$meta$n_save))
  nvars <- BVAR$variables
  for (r in 1:BVAR$meta$n_save) {
    A.draw <- BVAR$beta[r, , ]
    A.draw <- AR.coefficients(A.draw, BVAR$meta$M, BVAR$meta$lags)
    A.draw <- AR.companion(A.draw, BVAR$meta$M, BVAR$meta$lags, r)
    P.draw <- t(chol(BVAR$sigma[r, , ]))
    Fb[, , , r] <- fevd(A.draw, P.draw, BVAR$meta$M, H)
  }
  Fl <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1))
  Fc <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1))
  Fu <- array(NA, c(BVAR$meta$M, BVAR$meta$M, H + 1))
  for (h in 1:(H + 1)) {
    for (j in 1:BVAR$meta$M) {
      for (i in 1:BVAR$meta$M) {
        ci <- ci.efron(Fb[i, j, h, ], a)
        Fl[i, j, h] <- ci[1]
        Fc[i, j, h] <- ci[2]
        Fu[i, j, h] <- ci[3]
      }
    }
  }
  dimnames(Fc)[[1]] <- toupper(nvars)
  dimnames(Fc)[[2]] <- paste("S.", 1:BVAR$meta$M, sep = "")
  dimnames(Fc)[[3]] <- 0:H
  dimnames(Fl) <- dimnames(Fc)
  dimnames(Fu) <- dimnames(Fc)
  FEVD <- list(lb = Fl, pe = Fc, ub = Fu)
  return(FEVD)
}

BVAR.hd <- function(BVAR, a) {
  Hb <- array(NA, c(BVAR$meta$M, BVAR$meta$M + 1, BVAR$meta$N, BVAR$meta$n_save))
  nvars <- BVAR$variables
  for (r in 1:BVAR$meta$n_save) {
    B.draw <- BVAR$beta[r, , ]
    A.draw <- AR.coefficients(B.draw, BVAR$meta$M, BVAR$meta$lags)
    A.draw <- AR.companion(A.draw, BVAR$meta$M, BVAR$meta$lags, r)
    P.draw <- t(chol(BVAR$sigma[r, , ]))
    E.draw <- BVAR$meta$Y - BVAR$meta$X %*% B.draw
    Hb[, , , r] <- hd(A.draw, P.draw, BVAR$meta$Y, E.draw, BVAR$meta$M, BVAR$meta$N)
  }
  Hl <- array(NA, c(BVAR$meta$M, BVAR$meta$M + 1, BVAR$meta$N))
  Hc <- array(NA, c(BVAR$meta$M, BVAR$meta$M + 1, BVAR$meta$N))
  Hu <- array(NA, c(BVAR$meta$M, BVAR$meta$M + 1, BVAR$meta$N))
  for (t in 1:BVAR$meta$N) {
    for (j in 1:(BVAR$meta$M + 1)) {
      for (i in 1:BVAR$meta$M) {
        ci <- ci.efron(Hb[i, j, t, ], a)
        Hl[i, j, t] <- ci[1]
        Hc[i, j, t] <- ci[2]
        Hu[i, j, t] <- ci[3]
      }
    }
  }
  dimnames(Hc)[[1]] <- toupper(nvars)
  dimnames(Hc)[[2]] <- c(paste("S.", 1:BVAR$meta$M, sep = ""), "NS")
  dimnames(Hc)[[3]] <- 1:BVAR$meta$N
  dimnames(Hl) <- dimnames(Hc)
  dimnames(Hu) <- dimnames(Hc)
  HD <- list(lb = Hl, pe = Hc, ub = Hu)
  return(HD)
}

BVAR.erpt <- function(BVAR, vx, vy, H, a, cumulative = FALSE) {
  Eb <- array(NA, c(H + 1, BVAR$meta$n_save))
  nvars <- BVAR$variables
  for (r in 1:BVAR$meta$n_save) {
    B.draw <- BVAR$beta[r, , ]
    A.draw <- AR.coefficients(B.draw, BVAR$meta$M, BVAR$meta$lags)
    A.draw <- AR.companion(A.draw, BVAR$meta$M, BVAR$meta$lags, r)
    P.draw <- t(chol(BVAR$sigma[r, , ]))
    if (cumulative == FALSE) {
      Eb[, r] <- erpt(A.draw, P.draw, BVAR$meta$M, H, vx, vy, cumulative = FALSE)
    } else {
      Eb[, r] <- erpt(A.draw, P.draw, BVAR$meta$M, H, vx, vy, cumulative = TRUE)
    }
  }
  El <- array(NA, c(H + 1))
  Ec <- array(NA, c(H + 1))
  Eu <- array(NA, c(H + 1))
  for (h in 1:(H + 1)) {
    ci <- ci.efron(Eb[h, ], a)
    El[h] <- ci[1]
    Ec[h] <- ci[2]
    Eu[h] <- ci[3]
  }
  ERPT <- list(lb = El, pe = Ec, ub = Eu)
  return(ERPT)
}