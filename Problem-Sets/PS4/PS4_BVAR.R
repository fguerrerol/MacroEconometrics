remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

source("PS4_Data.R")

Yl.f <- cbind(pcom, er, pc)
Yl.f <- 100 * log(Yl.f) # log transformation
Yd.f <- diff(Yl.f) # log-diff transformation

remove(pcom, er, pc)

Yl <- window(Yl.f, start = c(2003, 01), end = c(2019, 12))
Yd <- window(Yd.f, start = c(2003, 01), end = c(2019, 12))

# BVAR Estimation (Reduced Form) ####

library(BVAR)

p <- 3 # lag order

Y <- Yd
Y <- ts(Y[(12 - p + 1):nrow(Y), ], end = end(Y), frequency = frequency(Y)) # Starting in Jan-04

# Prior Setup: Hyperparamenters
mean.l1 <- 0.3
var.det <- 1e07

# Prior Setup: A) Tight Lambda
pr.minn.A <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.001), alpha = bv_alpha(mode = 2), var = var.det, b = mean.l1)
pr.strc.A <- bv_priors(hyper = "auto", mn = pr.minn.A)

# Prior Setup: B) Loose Lambda
pr.minn.B <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.1), alpha = bv_alpha(mode = 2), var = var.det, b = mean.l1)
pr.strc.B <- bv_priors(hyper = "auto", mn = pr.minn.B)

# Simulation: Settings
draws <- 5000
burns <- 1000

# Simulation: A) Tight Lambda
BVAR.A <- bvar(data = Y, lags = p, n_draw = draws, n_burn = burns, priors = pr.strc.A, verbose = TRUE)
plot(density(BVAR.A$hyper), main = "Lambda (tight)")
plot(density(BVAR.A$beta[, 3, 3]), main = "Beta-32 (model with tight Lambda prior)")
plot(density(BVAR.A$beta[, 4, 3]), main = "Beta-33 (model with tight Lambda prior)")
plot(density(BVAR.A$sigma[, 3, 3]), main = "Sigma-33 (model with tight Lambda prior)")

# Simulation: B) Loose Lambda
BVAR.B <- bvar(data = Y, lags = p, n_draw = draws, n_burn = burns, priors = pr.strc.B, verbose = TRUE)
plot(density(BVAR.B$hyper), main = "Lambda (loose)")
plot(density(BVAR.B$beta[, 3, 3]), main = "Beta-32 (model with loose Lambda prior)")
plot(density(BVAR.B$beta[, 4, 3]), main = "Beta-33 (model with loose Lambda prior)")
plot(density(BVAR.B$sigma[, 3, 3]), main = "Sigma-33 (model with loose Lambda prior)")

# Forecasting ####
BVAR <- bvar(data = Y, lags = p, n_draw = draws, n_burn = burns, priors = pr.strc.B, verbose = TRUE)

H.forecast <- 24 # Forecast horizon

# Unconditional
forecast.U <- predict(BVAR, horizon = H.forecast, conf_bands = c(0.05, 0.1, 0.2))
plot(forecast.U, area = TRUE, t_back = 48)

# Conditional (on "er")
path <- rep(1, H.forecast / 2)
forecast.C <- predict(BVAR, horizon = H.forecast, cond_path = path, cond_var = "er", conf_bands = c(0.05, 0.1, 0.2))
plot(forecast.C, area = TRUE, t_back = 48)

# Bayesian SVAR Analysis ####

source("PS4_SVAR_Analysis.R")
source("PS4_BVAR_Tools.R")
source("PS4_BVAR_Plots.R")

gamma <- 0.95 # Confidence Level

H <- 12 # Horizon
H.ERPT <- 24 # Horizon for ERPT

# IRF
SIRF <- BVAR.sirf(BVAR, H, gamma, cumulative = FALSE)
plot.sirf.bvar(SIRF, BVAR$meta$M, H)

# Cumulative IRF
SIRF.C <- BVAR.sirf(BVAR, H, gamma, cumulative = TRUE)
plot.sirf.bvar(SIRF.C, BVAR$meta$M, H)

# FEVD
FEVD <- BVAR.fevd(BVAR, H, gamma)
plot.fevd.bvar(FEVD, BVAR$meta$M, H)

# HD
HD <- BVAR.hd(BVAR, gamma)
plot.hd.bvar(BVAR$meta$Y, HD, BVAR$meta$M, BVAR$meta$N)

# ERPT
ERPT <- BVAR.erpt(BVAR, 3, 2, H.ERPT, gamma, cumulative = TRUE)
plot.erpt.bvar(ERPT, H.ERPT)

# BVAR (log-levels) ####

p <- 3 # lag order

Y <- Yl
Y <- ts(Y[(12 - p + 1):nrow(Y), ], end = end(Y), frequency = frequency(Y)) # Starting in Jan-04

# Prior
pr.minn.B.lvl <- bv_minnesota(lambda = bv_lambda(mode = 0.2, sd = 0.1), alpha = bv_alpha(mode = 2), var = var.det)
pr.strc.B.lvl <- bv_priors(hyper = "auto", mn = pr.minn.B.lvl)

# Estimation
BVAR.B.lvl <- bvar(data = Y, lags = p, n_draw = draws, n_burn = burns, priors = pr.strc.B.lvl, verbose = TRUE)
plot(density(BVAR.B.lvl$hyper), main = "Lambda (loose)")
plot(density(BVAR.B.lvl$beta[, 3, 3]), main = "Beta-32 (model with loose Lambda prior)")
plot(density(BVAR.B.lvl$beta[, 4, 3]), main = "Beta-33 (model with loose Lambda prior)")
plot(density(BVAR.B.lvl$sigma[, 3, 3]), main = "Sigma-33 (model with loose Lambda prior)")

# Forecasting: Model
BVAR.lvl <- bvar(data = Y, lags = p, n_draw = draws, n_burn = burns, priors = pr.strc.B, verbose = TRUE)

# Forecasting: Unconditional
forecast.U <- predict(BVAR.lvl, horizon = H.forecast, conf_bands = c(0.05, 0.1, 0.2))
plot(forecast.U, area = TRUE, t_back = 48)

# Forecasting: Conditional (on "er")
path <- rep(100 * log(70), H.forecast / 2)
forecast.C <- predict(BVAR.lvl, horizon = H.forecast, cond_path = path, cond_var = "er", conf_bands = c(0.05, 0.1, 0.2))
plot(forecast.C, area = TRUE, t_back = 48)

# IRF
SIRF.lvl <- BVAR.sirf(BVAR.lvl, H, gamma, cumulative = FALSE)
plot.sirf.bvar(SIRF.lvl, BVAR$meta$M, H)

# FEVD
FEVD.lvl <- BVAR.fevd(BVAR.lvl, H, gamma)
plot.fevd.bvar(FEVD.lvl, BVAR$meta$M, H)

# ERPT
ERPT.lvl <- BVAR.erpt(BVAR.lvl, 3, 2, H.ERPT, gamma, cumulative = FALSE)
plot.erpt.bvar(ERPT.lvl, H.ERPT)