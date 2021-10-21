remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

source("PS1_Data.R")

# Unit Root Testing ####

library(urca) # "vars" already loads this library

z <- log(pcom)

# URT: Augmented-Dickey-Fuller
adf <- ur.df(y = z, type = "trend", lags = 2, selectlags = "Fixed")
summary(adf)

## URT: Elliott, Rothenberg & Stock
# ers <- ur.ers(y = z, type = "DF-GLS", model = "const", lag.max = 4)
# sumary(ers)
#
## URT: Kwiatkowski-Phillips-Schmidt-Shin
# kpss <- ur.kpss(y = z, type = "tau", lags = "short")
# summary(kpss)
#
## URT: Phillips & Perron
# pp <- ur.pp(y = z, type = "Z-tau", model = "trend", lags = "short")
# summary(pp)
#
## URT: Schmidt & Phillips
# sp <- ur.sp(y = z, type = "tau", pol.deg = 1, signif = 0.01)
# summary(sp)
#
## URT: Zivot & Andrews
# za <- ur.za(y = z, model = "both", lag = 2)
# summary(za)