remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ####

library(readr)

X <- read_csv("Data_LP.csv")
X <- ts(X, start = c(2004, 01), frequency = 4)
X <- window(X, end = c(2019, 04))

GC <- X[, "RGC"]
GS <- X[, "NGS"] / X[, "P"]
 Y <- X[, "Y"]

library(seasonal)

seas.adj <- seas(GS)
GS <- seas.adj$series$s11

X <- cbind(GC, Y) # VARIABLE ORDERING IS CRUCIAL!
X <- 100 * log(X) # log transformation
X <- diff(X) # log-diff transformation

remove(GC, GS, Y, seas.adj)

plot(X)

# LP Analysis ####

source("PS3_LP_Tools.R")

p <- 2 # lag length

H <- 4 # Horizon
gamma <- 0.95 # Confidence level

idx.s <- 1 # Shock: G
idx.r <- 2 # Response: Y

# IRF
LP <- lp(X, p, idx.s, idx.r, H, gamma)
plot.lp(LP)

# IRF (cumulative)
LP.c <- lp(X, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
plot.lp(LP.c)

D <- ifelse(X[, "Y"] < 0, 1, 0)

# Nonlinear IRF
LP.pw <- lp.pw(X, D, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
plot.lp.pw(LP.pw)

# Nonlinear IRF (cumulative)
LP.pw.c <- lp.pw(X, D, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
plot.lp.pw(LP.pw.c, c("Recession", "Otherwise"))