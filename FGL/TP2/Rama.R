### Código de Ramiro ######

### Completamente experimental $#####
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(xts)
library(readxl)
library(stringr)
library(dplyr)
library(urca)
library(vars)

####### PreProcesamiento y Armado de Base #######
# Creamos la base de datos para el IPC
source("1_Serie IPC.R")

# Creamos la base de datos - LIMPIAMOS LOS RESIDUOS, DESESTACIONALIZAMOS M2 -
source("2_ArmadoBase.R")

####### Ejercicio 1 #######

base = read.csv("output/base_en_diferencias.csv")
names(base)

# 3 Variables #
Y = base[,c("dl_ipex", "dl_tcn", "dl_ipc")]
Y <- ts(Y, start = c(2005, 01), frequency = 12)

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC

# Estimation
#Y <- ts(Y[(pmax - p + 1):nrow(Y), ], end = end(Y), frequency = frequency(Y))
VAR <- VAR(Y, p = p, type = "const")
summary(VAR)

m <- VAR$K #cantidad de variables


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


# PRECIOS INTERNACIONALES NO DEPENDEN DE VARIABLES INTERNAS..
VAR_r <- restrict(VAR, method = "man", resmat = matC(m, p, 1)) # Keeps only nonzero values

# A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

# B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

# RESTRICCIONES DE CORTO PLAZO RECURSIVA... 
# MATRIZ A DE EFECTOS CONTEMPORANEAS ES TRIANGULAR INFERIOR
SVAR <- SVAR(VAR_r, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR$A

source("PS2_Miscellaneous.R")
source("PS2_Bootstrap.R")
source("PS2_SVAR_Plots.R")
source("PS2_SVAR_Tools.R")

# Bootstrap Replications

H <- 12 # Horizon
H.ERPT <- 120 # Horizon (ERPT)

R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

Y.boot <- boot.replicate(VAR_r, R, type)

# IRF (Bootstrap)
#IRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot, cumulative = TRUE)
#plot.sirf.boot(IRF.boot, m, H) 

source("PS3_SVAR_LR.R")
source("PS3_SIRF_Transform.R")

# PASS THROUGH
# ERPT (Bootstrap)
I.er <- SVAR.sirf.lr.boot(SVAR, H.ERPT, gamma, Y.boot, cumulative = TRUE)
#I.pc <- trans.pw.boot("-", I.er, 2, 1, gamma)
#I <- list(lb = abind(I.er$lb, I.pc$lb, along = 1), pe = abind(I.er$pe, I.pc$pe, along = 1), ub = abind(I.er$ub, I.pc$ub, along = 1), boot = abind(I.er$boot, I.pc$boot, along = 2))

ERPT.boot <- trans.pw.boot("/", I.er, 3, 2, gamma)

dim(ERPT.boot$lb)
#ERPT.R.boot <- list(lb = 100 * ERPT.boot$lb[1, ], pe = 100 * ERPT.boot$pe[1, ], ub = 100 * ERPT.boot$ub[1, ], boot = 100 * ERPT.boot$boot[, 1, ])
ERPT.boot_S2 <- list(lb = 100 * ERPT.boot$lb[2, ], pe = 100 * ERPT.boot$pe[2, ], ub = 100 * ERPT.boot$ub[2, ], boot = 100 * ERPT.boot$boot[, 2, ])

#remove(I.er, I.pc, I, ERPT.boot)
par(mfrow = c(1,1))
plot.erpt.boot(ERPT.boot_S2, H.ERPT)
################################

# 2 Variables #
Y = base[,c("dl_tcn", "dl_ipc")]
Y <- ts(Y, start = c(2005, 01), frequency = 12)

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC

# Estimation
#Y <- ts(Y[(pmax - p + 1):nrow(Y), ], end = end(Y), frequency = frequency(Y))
VAR <- VAR(Y, p = p, type = "const")
summary(VAR)

m <- VAR$K #cantidad de variables

# A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

# B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

# RESTRICCIONES DE CORTO PLAZO RECURSIVA... 
# MATRIZ A DE EFECTOS CONTEMPORANEAS ES TRIANGULAR INFERIOR
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR$A

# Bootstrap Replications

H <- 12 # Horizon
H.ERPT <- 120 # Horizon (ERPT)

R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

Y.boot <- boot.replicate(VAR, R, type)

# IRF (Bootstrap)
#IRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot, cumulative = TRUE)
#plot.sirf.boot(IRF.boot, m, H) 

# PASS THROUGH
# ERPT (Bootstrap)
I.er <- SVAR.sirf.lr.boot(SVAR, H.ERPT, gamma, Y.boot, cumulative = TRUE)
#I.pc <- trans.pw.boot("-", I.er, 2, 1, gamma)
#I <- list(lb = abind(I.er$lb, I.pc$lb, along = 1), pe = abind(I.er$pe, I.pc$pe, along = 1), ub = abind(I.er$ub, I.pc$ub, along = 1), boot = abind(I.er$boot, I.pc$boot, along = 2))

ERPT.boot <- trans.pw.boot("/", I.er, 2, 1, gamma)

dim(ERPT.boot$lb)
#ERPT.R.boot <- list(lb = 100 * ERPT.boot$lb[1, ], pe = 100 * ERPT.boot$pe[1, ], ub = 100 * ERPT.boot$ub[1, ], boot = 100 * ERPT.boot$boot[, 1, ])
ERPT.boot_S2 <- list(lb = 100 * ERPT.boot$lb[1, ], pe = 100 * ERPT.boot$pe[1, ], ub = 100 * ERPT.boot$ub[1, ], boot = 100 * ERPT.boot$boot[, 1, ])

#remove(I.er, I.pc, I, ERPT.boot)
par(mfrow = c(1,1))
plot.erpt.boot(ERPT.boot_S2, H.ERPT)
################################