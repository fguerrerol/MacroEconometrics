remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

source("PS1_Data.R")

Yl <- cbind(pcom, er, pc)
Yl <- log(Yl) # log transformation
Yl <- window(Yl, start = c(2004, 01), end = c(2019, 12)) # Keep specific observations
Yd <- 100 * diff(Yl) # log-diff transformation

plot(Yl) # Plotting the variables (log-levels)
plot(Yd) # Plotting the variables (log-differences)

# VAR Estimation ####

library(vars)

# Lag Order Selection
pmax <- 6 # Maximum lag order

popt <- VARselect(Yd, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC

# Estimation
VAR <- VAR(Yd, p = p, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR)

m <- VAR$K # Number of variables in the VAR
T <- VAR$obs # Number of effective sample observations, excluding "p" starting values

plot(VAR)

# Manual plotting of residuals
# e <- resid(VAR)
# e <- ts(e, end = end(Yd), frequency = frequency(Yd))
# colnames(e) <- paste("e.", colnames(Yd), sep = "")
# plot(e, main = "Residuals")

# Granger Causality ####

# GC Test, Local Vars. -> PCOM (asymptotic) 
VAR.GC.test.asym <- causality(VAR, cause = c("er", "pc"))
VAR.GC.test.asym

# GC Test, Local Vars. -> PCOM (bootstrap)
VAR.GC.test.boot <- causality(VAR, cause = c("er", "pc"), boot = TRUE, boot.runs = 2000)
VAR.GC.test.boot

# Diagnostics Tests ####

# VAR stability

# Eigenvalues
VAR.roots <- roots(VAR, modulus = TRUE)
VAR.roots

# Residual Serial Correlation

h.PT <- min(10, trunc(T / 5)) # Rule of thumb for Portmanteau tests (Rob Hyndman) # https://robjhyndman.com/hyndsight/ljung-box-test/
# Portmanteau Test
VAR.PT.test.serial <- serial.test(VAR, lags.pt = h.PT, type = "PT.asymptotic")
VAR.PT.test.serial

# Portmanteau Test (adjusted)
VAR.PT.test.serial.adj <- serial.test(VAR, lags.pt = h.PT, type = "PT.adjusted") # Small sample correc.
VAR.PT.test.serial.adj

h.BG <- 3
# Breusch-Godfrey Test
VAR.BG.test.serial <- serial.test(VAR, lags.bg = h.BG, type = "BG")
VAR.BG.test.serial

# Breusch-Godfrey Test (adjusted)
VAR.BG.test.serial.adj<- serial.test(VAR, lags.bg = h.BG, type = "ES") # Small sample correc.
VAR.BG.test.serial.adj

# Residual Normality

# Multivariate Jarque-Bera Test
VAR.JB.test <- normality.test(VAR, multivariate.only = FALSE)
VAR.JB.test

# Residual Heteroskedasticity

q <- 3
# Multivariate ARCH Test
VAR.ARCH.test <- arch.test(VAR, lags.multi = 12, multivariate.only = FALSE)
VAR.ARCH.test

# Ad hoc function (checks everything at once for different lag orders)
fast.check <- function(Y, p, pmax, T, h.PT, h.BG, q, X = NULL) {
  CHK <- data.frame(matrix(NA, pmax, 8))
  CHK[, 1] <- 1:pmax
  if (is.null(X)) {
    for (l in 1:pmax) {
      # Yt <- Y
      Yt <- Y[(pmax - l + 1):T + p, ]
      VARt <- VAR(Yt, p = l, type = "const")
      PT.a <- serial.test(VARt, lags.pt = h.PT, type = "PT.adjusted")
      BG.a <- serial.test(VARt, lags.bg = h.BG, type = "ES")
      JB <- normality.test(VARt, multivariate.only = TRUE)
      ARCH <- arch.test(VARt, lags.multi = q, multivariate.only = TRUE)
      CHK[l, 2] <- max(roots(VARt, modulus = TRUE))
      CHK[l, 3] <- PT.a$serial[3]
      CHK[l, 4] <- BG.a$serial[3]
      CHK[l, 5] <- JB$jb.mul[1]$JB[3]
      CHK[l, 6] <- JB$jb.mul[2]$Skewness[3]
      CHK[l, 7] <- JB$jb.mul[3]$Kurtosis[3]
      CHK[l, 8] <- ARCH$arch.mul[3]
    }
  } else {
    for (l in 1:pmax) {
      # Yt <- Y
      Yt <- Y[(pmax - l + 1):T + p, ]
      VARt <- VAR(Yt, p = l, type = "const", exogen = X)
      PT.a <- serial.test(VARt, lags.pt = h.PT, type = "PT.adjusted")
      BG.a <- serial.test(VARt, lags.bg = h.BG, type = "ES")
      JB <- normality.test(VARt, multivariate.only = TRUE)
      ARCH <- arch.test(VARt, lags.multi = q, multivariate.only = TRUE)
      CHK[l, 2] <- max(roots(VARt, modulus = TRUE))
      CHK[l, 3] <- PT.a$serial[3]
      CHK[l, 4] <- BG.a$serial[3]
      CHK[l, 5] <- JB$jb.mul[1]$JB[3]
      CHK[l, 6] <- JB$jb.mul[2]$Skewness[3]
      CHK[l, 7] <- JB$jb.mul[3]$Kurtosis[3]
      CHK[l, 8] <- ARCH$arch.mul[3]
    }
  }
  CHK <- round(CHK, 3)
  CHK <- data.frame(CHK)
  colnames(CHK) <- c("Lag", "MEV", "PT.adj", "BG.adj", "JB", "JB-S", "JB-K", "ARCH")
  return(CHK)
}

fast.check(Yd, p, pmax, T, h.PT, h.BG, q)

# Other ####

# Stability Analysis (Structural Change)
stab.test <- stability(VAR, type = "fluctuation")
plot(stab.test)

# Formal Test
sctest(stab.test$stability$er) # https://cran.r-project.org/web/packages/strucchange/

# Model Simplification ####

# Ad hoc function
matC <- function(m, p, vx) {
  vy <- setdiff(1:m, vx)
  Cm <- matrix(1, m, m * p + 1)
  for (i in vx) {
    for (l in 1:p) {
      for (j in vy) {
        Cm[i, m * (l - 1) + j] <- 0
      }
    }
  }
  return(Cm)
}

# Estimate VAR with zero constraints
VAR <- restrict(VAR, method = "man", resmat = matC(m, p, 1)) # Keeps only nonzero values
VAR

# Forecasting ####

H <- 12 # Forecast horizon
a <- 0.95 # Confidence level

# VAR Forecasts
VAR.fcst <- predict(VAR, n.head = H, ci = a) # Residual normality assumption is relevant for confidence bands
VAR.fcst

# Plotting Forecasts
plot(VAR.fcst)
fanchart(VAR.fcst)

# Plotting Inflation Forecast
plot(VAR.fcst, names = "pc")
fanchart(VAR.fcst, names = "pc")

# # Manual computation of Wald Test via SUR model
# 
# library(systemfit)
# 
# # Data and regressors
# y_pcom <-  as.numeric(Yd[, "pcom"])
# y_er <- as.numeric(Yd[, "er"])
# y_pc <- as.numeric(Yd[, "pc"])
# 
# y_pcom_l1 <- y_pcom[1:T]
# y_er_l1 <- y_er[1:T]
# y_pc_l1 <- y_pc[1:T]
# 
# y_pcom <-  y_pcom[(p + 1):(T + p)]
# y_er <- y_er[(p + 1):(T + p)]
# y_pc <- y_pc[(p + 1):(T + p)]
# 
# # SUR equations
# eqpcom <- y_pcom ~ y_pcom_l1 + y_er_l1 + y_pc_l1
# eqer <- y_er ~ y_pcom_l1 + y_er_l1 + y_pc_l1
# eqpc <- y_pc ~ y_pcom_l1 + y_er_l1 + y_pc_l1
# 
# # SUR estimation
# system <- list(pcomreg = eqpcom, erreg = eqer, pcreg = eqpc)
# fitsur <- systemfit(system, method = "OLS")
# summary(fitsur)
# 
# # Constraints for Granger Causality Test
# Rmat <- matrix(0, nrow = 1, ncol = 12)
# Rmat[1, 11] <- 1
# qvec <- c(0)
# 
# # Granger Causality Test, ER -> PC (Manual)
# linearHypothesis(fitsur, Rmat, qvec, test = "F")