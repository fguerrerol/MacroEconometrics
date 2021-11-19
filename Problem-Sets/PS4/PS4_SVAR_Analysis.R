sirf <- function(Phi, P, m, H, cumulative = FALSE) {
  Psi <- array(NA, c(m, m, H + 1))
  Psi[, , 1] <- P
  tmp_Phi <- Phi
  for (h in 1:H) {
    Psi[, , h + 1] <- tmp_Phi[1:m, 1:m] %*% P
    tmp_Phi <- tmp_Phi %*% Phi
  }
  if (cumulative == FALSE) {
    return(Psi)
  } else {
    return(aperm(apply(Psi, c(1, 2), cumsum), c(2, 3, 1)))
  }
}

fevd <- function(Phi, P, m, H) {
  Psi <- sirf(Phi, P, m, H)
  Omega <- array(NA, c(m, m, H + 1))
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        Omega[i, j, h] <- sum(Psi[i, j, 1:h] ^ 2)
      }
    }
  }
  MSE <- apply(Omega, c(1, 3), sum)
  for (h in 1:(H + 1)) {
    for (j in 1:m) {
      for (i in 1:m) {
        Omega[i, j, h] <- Omega[i, j, h] / MSE[i, h]
      }
    }
  }
  return(100 * Omega)
}

hd <- function(Phi, P, Y, E, m, N) {
  Psi <- sirf(Phi, P, m, N)
  U <- t(solve(P, t(E)))
  HD <- array(NA, c(m, m + 1, N))
  for (t in 1:N) {
    for (j in 1:m) {
      for (i in 1:m) {
        HD[i, j, t] <- c(crossprod(Psi[i, j, 1:t], U[t:1, j]))
      }
    }
  }
  NS <- Y - t(apply(HD[, 1:m, ], c(1, 3), sum))
  for (i in 1:m) {
    HD[i, m + 1, ] <- NS[, i]
  }
  return(HD)
}

erpt <- function(Phi, P, m, H, vx, vy, cumulative = FALSE) {
  if (cumulative == FALSE) {
    I <- sirf(Phi, P, m, H)
  } else {
    I <- sirf(Phi, P, m, H, cumulative = TRUE)
  }
  return(100 * I[vx, vy, ] / I[vy, vy, ])
}