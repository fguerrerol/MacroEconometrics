remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(VARsignR)

# Data
data(uhligdata)

# Sign Restrictions
sign.C <- c(+4, -3, -2, -5) # ***
# *** First variables should be the shock of interest

# Labels
vl <- c("GDP", "GDP Deflator", "Comm. Price Index", "Fed Funds Rate", "NB Reserves", "Total Reserves")

# Model Estimation
BVAR <- rwz.reject(Y = uhligdata, nlags = 12, draws = 200, subdraws = 200, nkeep = 1000, KMIN = 1, KMAX = 6, constrained = sign.C, constant = FALSE, steps = 60)

# Confidence Bands
ci <- c(0.05, 0.95)

# IRFs
irfplot(irfdraws = BVAR$IRFS, type = "median", labels = vl, save = FALSE, bands = ci, grid = TRUE, bw = FALSE)

# FEVDs
fevdplot(fevddraws = BVAR$FEVDS, label = vl, save = FALSE, bands = ci, grid = TRUE, bw = FALSE, table = FALSE, periods = NULL)