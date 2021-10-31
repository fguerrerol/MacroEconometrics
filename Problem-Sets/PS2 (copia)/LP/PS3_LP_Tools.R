source("PS3_LP_Plots.R")

library(sandwich)

get.shock <- function(Y, p, m, idx.s) {
  Wc <- embed(Y, p + 1)
  X1 <- Wc[, (m + 1):(m * (p + 1))]
  if (idx.s == 1) {
    X0 <- NULL
  } else {
    X0 <- Wc[, -c(idx.s:m)]
  }
  Xt <- cbind(1, X0, X1)
  yt <- Wc[, idx.s]
  unname(resid(lm(yt ~ -1 + Xt)))
}

get.shock.pw <- function(Y, D, p, m, idx.s) {
  Wc <- embed(Y, p + 1)
  X1 <- Wc[, (m + 1):(m * (p + 1))]
  if (idx.s == 1) {
    X0 <- NULL
  } else {
    X0 <- Wc[, -c(idx.s:m)]
  }
  Dt <- D[(p + 1):nrow(Y)]
  Xt <- cbind(1, X0, X1)
  Xt <- cbind(Dt * Xt, (1 - Dt) * Xt)
  Yt <- Wc[, idx.s]
  Yt <- cbind(Dt * Yt, (1 - Dt) * Yt)
  unname(resid(lm(Yt ~ -1 + Xt)))
}

lp <- function(Y, p, idx.s, idx.r, H, gamma, cumulative = FALSE) {
  
  m <- ncol(Y)
  T <- nrow(Y)
  
  rsp <- Y[, idx.r]
  imp <- get.shock(Y, p, m, idx.s)
  
  irf.pe <- rep(NA, H + 1)
  irf.lb <- rep(NA, length(irf.pe))
  irf.ub <- rep(NA, length(irf.pe))
  
  z <- qnorm(1 - (1 - gamma) / 2)
  
  for (h in 0:H) {
    
    rh <- rsp[(h + p + 1):T]
    st <- imp[1:(T - p - h)]
    
    if (isTRUE(cumulative) && h > 0) {
      rh <- diff(cumsum(c(0, rh)), h + 1)
      st <- st[(h + 1):length(st)]
    }
    
    projection <- lm(rh ~ -1 + st)
    
    b.pe <- unname(coef(projection))
    b.se <- unname(sqrt(diag(vcovHAC(projection))))
    
    irf.pe[h + 1] <- b.pe
    irf.lb[h + 1] <- b.pe - z * b.se
    irf.ub[h + 1] <- b.pe + z * b.se
    
  }
  
  list(lb = irf.lb, pe = irf.pe, ub = irf.ub)
  
}

lp.pw <- function(Y, D, p, idx.s, idx.r, H, gamma, cumulative = FALSE) {
  
  m <- ncol(Y)
  T <- nrow(Y)
  
  rsp <- Y[, idx.r]
  imp <- get.shock.pw(Y, D, p, m, idx.s)
  
  irf.pe <- matrix(NA, H + 1, ncol(imp))
  irf.lb <- array(NA, dim(irf.pe))
  irf.ub <- array(NA, dim(irf.pe))
  
  z <- qnorm(1 - (1 - gamma) / 2)
  
  for (h in 0:H) {
    
    rh <- rsp[(h + p + 1):T]
    St <- imp[1:(T - p - h), ]
    
    if (isTRUE(cumulative) && h > 0) {
      rh <- diff(cumsum(c(0, rh)), h + 1)
      St <- St[(h + 1):nrow(St), ]
    }
    
    projection <- lm(rh ~ -1 + St)
    
    b.pe <- unname(coef(projection))
    b.se <- unname(sqrt(diag(vcovHAC(projection))))
    
    irf.pe[h + 1, ] <- b.pe
    irf.lb[h + 1, ] <- b.pe - z * b.se
    irf.ub[h + 1, ] <- b.pe + z * b.se
    
  }
  
  list(lb = irf.lb, pe = irf.pe, ub = irf.ub)
  
}