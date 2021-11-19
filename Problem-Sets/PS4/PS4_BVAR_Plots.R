plot.sirf.bvar <- function(X, m, H) {
  for (i in 1:m) {
    par(mfrow = c(m, 1))
    for (j in 1:m) {
      plot(0:H, X$pe[i, j, ],
           main = paste("Response of", dimnames(X$pe)[[1]][i], "to", dimnames(X$pe)[[2]][j], "shock", sep = " "),
           xlab = "Horizon", ylab = "",
           ylim = c(min(X$lb[i, j, ]), max(X$ub[i, j, ])),
           type = "o", lwd = 2)
      grid(NULL, NULL, lty = 1)
      xx <- c(0:H, H:0)
      yy <- c(c(X$lb[i, j, ]), rev(c(X$ub[i, j, ])))
      polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
    }
  }
}

plot.fevd.bvar <- function(X, m, H) {
  for (i in 1:m) {
    par(mfrow = c(m, 1))
    for (j in 1:m) {
      plot(0:H, X$pe[i, j, ],
           main = paste("Contribution of", dimnames(X$pe)[[2]][j], "shock to variance of", dimnames(X$pe)[[1]][i], sep = " "),
           xlab = "Horizon", ylab = "%",
           ylim = c(min(X$lb[i, j, ]), max(X$ub[i, j, ])),
           type = "o", lwd = 2)
      grid(NULL, NULL, lty = 1)
      xx <- c(0:H, H:0)
      yy <- c(c(X$lb[i, j, ]), rev(c(X$ub[i, j, ])))
      polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
    }
  }
}

plot.hd.bvar <- function(Y, X, m, N) {
  for (i in 1:m) {
    par(mfrow = c(m + 1, 1))
    for (j in 1:(m + 1)) {
      plot(1:N, X$pe[i, j, ],
           main = paste("Influence of", dimnames(X$pe)[[2]][j], "in", dimnames(X$pe)[[1]][i], sep = " "),
           xlab = "Time", ylab = "",
           ylim = c(min(X$lb[i, j, ], Y[, i]), max(X$ub[i, j, ], Y[, i])),
           type = "l", lwd = 1)
      lines(Y[, i], col = "red", lwd = 2)
      grid(NULL, NULL, lty = 1)
      xx <- c(1:N, N:1)
      yy <- c(c(X$lb[i, j, ]), rev(c(X$ub[i, j, ])))
      polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
    }
  }
}

plot.erpt.bvar <- function(X, H) {
  par(mfrow = c(1, 1))
  plot(0:H, X$pe,
       main = "Exchange-rate pass-through to consumer prices",
       xlab = "Horizon", ylab = "%",
       ylim = c(min(X$lb), max(X$ub)),
       type = "o", lwd = 2)
  grid(NULL, NULL, lty = 1)
  xx <- c(0:H, H:0)
  yy <- c(c(X$lb), rev(c(X$ub)))
  polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
}