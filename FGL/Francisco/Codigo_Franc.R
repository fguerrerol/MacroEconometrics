  library("readxl")
  library("tstools")
  library("xts")
  library("zoo")
  library("vars")
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  source("PS2_SVAR_Tools.R")
  source("PS2_SVAR_Plots.R")
  setwd("/home/francisco/Documentos/MEcon/T3/MacroEconometrics/Ejercitaciones/Ej1/Series")

  df <- read_excel("data0.xlsx")
  my_data = ts(df, start = c(2001, 1) , end=c(2019,12), frequency = 12)
  
  plot (my_data[,2])
  
  ### Conversi?n de los datos ###
  # precios del cobre
  my_data[,c(3)] <- log(my_data[,c(3)])*100
  
  # Imacec no minero
  my_data[,c(5)] <- log(my_data[,c(5)])*100
  
  # IPC SAE
  my_data[,c(6)] <- log(my_data[,c(6)])*100
  
  # TCN
  my_data[,c(7)] <- log(my_data[,c(7)])*100
  
  #### Punto 2 ####
  ### graficos en niveles ###
  
  par(mfrow=c(4,2),mar=c(2, 2, 2, 2))
  ts.plot(my_data[,c(2)], type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Tasa de Politica Monetaria de EEUU") 
  ts.plot(my_data[,c(3)], type="l", lwd=2, xlab="",ylab="$",bty="n", main = "Precio del cobre en ln por 100") 
  ts.plot(my_data[,c(4)], type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Incide EMBI Global") 
  ts.plot(my_data[,c(5)], type="l", lwd=2, xlab="",ylab="",bty="n", main = "IMACEC no minero en ln por 100") 
  ts.plot(my_data[,c(6)], type="l", lwd=2, xlab="",ylab="",bty="n", main = "IPC SAE en ln por 100") 
  ts.plot(my_data[,c(7)], type="l", lwd=2, xlab="",ylab="$",bty="n", main = "TCN en ln por 100") 
  ts.plot(my_data[,c(8)], type="l", lwd=2, xlab="",ylab="",bty="n", main = "Indice EMBI Chile ") 
  ts.plot(my_data[,c(9)], type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Tasa de Politica Monetaria de Chile") 
  
  
  ### tests de raiz unitaria ###
  library(urca)
  
  
  ### r* es I(1) ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,2]),type="none",selectlags="BIC")) #rechaza h0 al 10%
  summary(ur.df((my_data[,2]),type="drift",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,2]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,2])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,2])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,2])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  ### ln(precio del cobre)*100 es I(1) ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,3]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,3]),type="drift",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,3]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,3])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,3])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,3])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  ### Indice EMBI Global es I(1) ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,4]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,4]),type="drift",selectlags="BIC")) # rechaza h0 al 10%
  summary(ur.df((my_data[,4]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,4])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,4])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,4])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  ### ln ( Indice IMACEC no minero, desestacionalizado) *100 es I(1) ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,5]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,5]),type="drift",selectlags="BIC")) # rechaza h0 al 10%
  summary(ur.df((my_data[,5]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,5])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,5])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,5])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  ### ln (IPC SAE )* 100 es I(1) ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,6]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,6]),type="drift",selectlags="BIC")) # # no rechaza h0
  summary(ur.df((my_data[,6]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,6])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,6])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,6])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  ### ln (TCN)* 100 es I(1) ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,7]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,7]),type="drift",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,7]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,7])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,7])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,7])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  ### Indice EMBI Chile es I(1) al nivel del 1%, es I(0) al nivel 5% ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,8]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,8]),type="drift",selectlags="BIC")) # rechaza h0 al 5%
  summary(ur.df((my_data[,8]),type="trend",selectlags="BIC")) # rechaza h0 al 10%
  
  summary(ur.df((diff(my_data[,8])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,8])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,8])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  
  summary(ur.pp(my_data[,8], type="Z-tau", model="trend",lags="long"))
  summary(ur.pp(diff(my_data[,8]), type="Z-tau", model="constant",lags="long"))
  
  summary(ur.kpss(my_data[,8],type="tau",lags="long"))
  summary(ur.kpss(diff(my_data[,8]),type="mu",lags="long"))
  
  
  ### Tasa de Politica Monetaria de Chile es I(1) al 1%, I(0) al 5% ###
  
  ### ADF (Augmented Dickey-Fuller)  h0 = no estacionariedad ###
  summary(ur.df((my_data[,9]),type="none",selectlags="BIC")) # no rechaza h0
  summary(ur.df((my_data[,9]),type="drift",selectlags="BIC")) # rechaza h0 al 5%
  summary(ur.df((my_data[,9]),type="trend",selectlags="BIC")) # no rechaza h0
  
  summary(ur.df((diff(my_data[,9])),type="none",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,9])),type="drift",selectlags="BIC")) # rechaza h0 al 1%
  summary(ur.df((diff(my_data[,9])),type="trend",selectlags="BIC")) # rechaza h0 al 1%
  
  ### graficos de series en diferencias ###
  
  par(mfrow=c(4,2),mar=c(2, 2, 2, 2))
  ts.plot(diff(my_data[,c(2)]), type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Tasa de Politica Monetaria de EEUU") 
  ts.plot(diff(my_data[,c(3)]), type="l", lwd=2, xlab="",ylab="$",bty="n", main = "Precio del cobre en ln por 100") 
  ts.plot(diff(my_data[,c(4)]), type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Incide EMBI Global") 
  ts.plot(diff(my_data[,c(5)]), type="l", lwd=2, xlab="",ylab="",bty="n", main = "IMACEC no minero en ln por 100") 
  ts.plot(diff(my_data[,c(6)]), type="l", lwd=2, xlab="",ylab="",bty="n", main = "IPC SAE en ln por 100") 
  ts.plot(diff(my_data[,c(7)]), type="l", lwd=2, xlab="",ylab="$",bty="n", main = "TCN en ln por 100") 
  ts.plot(diff(my_data[,c(8)]), type="l", lwd=2, xlab="",ylab="",bty="n", main = "Indice EMBI Chile ") 
  ts.plot(diff(my_data[,c(9)]), type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Tasa de Politica Monetaria de Chile") 
  
  # hay que incluir las series en diferencias para que sean estacionarias
  
  
  #### Punto 3 version Mariana ####
  diff_imacec.log <-diff(my_data[,c(5)])
  dum2 <- data.frame(time=time(diff_imacec.log), 
                     Value=as.matrix(diff_imacec.log))
  dum2 <- as.character.Date(dum2)
  dum2$d1 <- ifelse(dum2$time=="2010.167",1,0)
  dum2$d2 <- ifelse(dum2$time=="2010.250",1,0)
  dum2$d3 <- ifelse(dum2$time=="2019.917",1,0)
  dum2$d4 <- ifelse(dum2$time=="2019.750",1,0)
  
  reg2 <- lm(dum2$Value~dum2$d1+dum2$d2+dum2$d3+dum2$d4)
  imacec <- reg2$residuals
  imacec <- ts(imacec, start=c(2001,02), frequency = 12)
  
  par(mfrow=c(3,1))
  ts.plot(imacec, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Ellos") 
  ts.plot(imacec_limpio, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Nuestro") 
  ts.plot(imacec_limpio - imacec , type="l", lwd=2, xlab="",ylab="",bty="n", main = "resta") 
  
  # Para el documento 
  descarte <- ts(reg2[["fitted.values"]], start=c(2001,02),end=c(2019,12), frequency=12)
  
  
  par(mfrow=c(3,1))
  ts.plot(diff(my_data[,c(5)]), type="l", lwd=2, xlab="",ylab="",bty="n", main = "Imacec no minero en ln por 100") 
  ts.plot(imacec, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Residuos - Imacec no minero limpio") 
  ts.plot(descarte, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Terremoto y conflicto social sobre Imacec no minero") 
  
  
  #### punto 4 ####
  
  # Modelo 1 : la internacional es TPM_USA
  
  tpm_usa     = ts(diff(my_data[,c(2)]),start=c(2001,02),frequency=12)
  embi_global = ts(diff(my_data[,c(4)]),start=c(2001,02),frequency=12)
  cobre       = ts(diff(my_data[,c(3)]),start=c(2001,02),frequency=12)
  tpm_chile =   ts(diff(my_data[,c(9)]),start=c(2001,02),frequency=12)
  embi_chile =  ts(diff(my_data[,c(8)]),start=c(2001,02),frequency=12)
  tcn =         ts(diff(my_data[,c(7)]),start=c(2001,02),frequency=12)
  ipc_sae =     ts(diff(my_data[,c(6)]),start=c(2001,02),frequency=12)
  
  
  modelo_1 <- cbind(tpm_usa, 
                    imacec,
                    ipc_sae, tcn,
                    embi_chile, tpm_chile)
  
  library(vars)
  var_1 <- VARselect(modelo_1, lag.max = 15, type = "const")
  var_1
  p<- var_1$selection[1] # AIC(n)
  
  
  # El mejor modelo uno es un VAR(3) segun AIC(n)
  VAR1 <- VAR(modelo_1, p = p, type = "const")
  summary(VAR1)
  
  
  
  m <- VAR1$K # Number of variables in the VAR
  T <- VAR1$obs # Number of effective sample observations, excluding "p" starting values
  
  
  
  # Manual plotting of residuals
  e <- resid(VAR1)
  e <- ts(e, end = end(modelo_1), frequency = frequency(modelo_1))
  colnames(e) <- c(1,2,3,4,5,6)
  plot(e, main = "Residuals")
  
  
  
  granger_boot_modelo1 <- causality(VAR1, cause = c("ipc_sae",
                                                "imacec",
                                                 "tcn",
                                                "embi_chile",
                                                "tpm_chile")
                                , boot = TRUE, boot.runs = 2000)
  granger_boot_modelo1
  
  
  
  matC <- function(m, p, vx) {
    Cm <- matrix(1, m, m * p + 1)
    listos <- vector()
    for (i in vx) {
      vy <- setdiff(1:m,i)
      vy <- setdiff(vy,listos)
      for (l in 1:p) {
        for (j in vy) {
          Cm[i, m * (l - 1) + j] <- 0
        }
      }
      listos <- c(listos,i)
    }
    Cm
  }
  
  
  Cm <- matrix(1, 3, 3 * 2 + 1)
  m = 3
  
  vy <- setdiff(1:m, vx)
  
  
  
  
Cmconstraints <- matC(m, p, 1)
  VAR1a <- restrict(VAR1, method = "man", resmat = constraints)
  VAR1a
  
  
  roots(VAR1a, modulus = TRUE)
  

  modelo <- cbind(tpm_usa,
                  embi_global,
                  cobre,
                  )

  
  
  ######
  # VAR Estimation (Structural Form) ####
  
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
  pars.R <- Bcoef(VAR1a) # Reduced Form VAR
  pars.S <- solve(SVAR$A, pars.R) # Structural Form VAR
  pars.R
  pars.S
  # SVAR Estimation (AB model configuration)
  SVAR1 <- SVAR(VAR1a, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
  SVAR1
  
  P <- solve(SVAR$A, SVAR$B) # inv(A) %*% B
  S.SVAR <- P %*% t(P)
  S.SVAR
  
  
  H <- 24 # Horizon
  H.ERPT <- 240 # Horizon (ERPT)
  
  # IRF

  R <- 500 # Number of bootstrap replications
  type <- "nonparametric"
  gamma <- 0.95 # Confidence level
  
  # COMMENT ON THE IMPORTANTE OF MULTIVARIATE CONFIDENCE INTERVALS 

# Bootstrap Replications
Y.boot <- boot.replicate(VAR1a, R, type)



IRF1.boot <- SVAR.sirf.boot(SVAR1, Amat, Bmat, H, gamma, Y.boot)
get.sirf.boot(IRF1.boot, m, H)





###### Modelo 2#######

modelo_2 <- cbind(cobre, 
                  imacec,
                  ipc_sae, tcn,
                  embi_chile, tpm_chile)

library(vars)
var_2 <- VARselect(modelo_2, lag.max = 15, type = "const")
var_2
p<- var_2$selection[1] # AIC(n)


# El mejor modelo uno es un VAR(3) segun AIC(n)
VAR2 <- VAR(modelo_2, p = p, type = "const")
summary(VAR2)



m <- VAR2$K # Number of variables in the VAR
T <- VAR2$obs # Number of effective sample observations, excluding "p" starting values



# Manual plotting of residuals
e <- resid(VAR1)
e <- ts(e, end = end(modelo_2), frequency = frequency(modelo_2))
colnames(e) <- c(1,2,3,4,5,6)
plot(e, main = "Residuals")



granger_boot_modelo2 <- causality(VAR2, cause = c("ipc_sae",
                                              "imacec",
                                              "tcn",
                                              "embi_chile",
                                              "tpm_chile")
                              , boot = TRUE, boot.runs = 2000)
granger_boot_modelo2




constraints <- matC(m, p, 1)
VAR2a <- restrict(VAR2, method = "man", resmat = constraints)
VAR2a


roots(VAR2a, modulus = TRUE)


source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")


######
# VAR Estimation (Structural Form) ####

# SVAR Estimation (AB model configuration)
SVAR2 <- SVAR(VAR2a, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR2



P <- solve(SVAR$A, SVAR$B) # inv(A) %*% B
S.SVAR <- P %*% t(P)
S.SVAR


H <- 24 # Horizon
H.ERPT <- 240 # Horizon (ERPT)

# IRF


R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

# COMMENT ON THE IMPORTANTE OF MULTIVARIATE CONFIDENCE INTERVALS 

# Bootstrap Replications
Y.boot <- boot.replicate(VAR2a, R, type)



IRF2.boot <- SVAR.sirf.boot(SVAR2, Amat, Bmat, H, gamma, Y.boot)
get.sirf.boot(IRF2.boot, m, H)


########################################
######## Modelo 3 - Embi Global#########



#### Inicializco el modelo apilando columnas

modelo_3 <- cbind(embi_global, 
                  imacec,
                  ipc_sae, tcn,
                  embi_chile, tpm_chile)

#### Selecciono el número apropiado de rezagos 
var_3 <- VARselect(modelo_3, lag.max = 15, type = "const")
var_3
p<- var_3$selection[1] # AIC(n)

### Inicializo el VAR Con el número de rezagos adecuados provisto por VARselect
VAR3 <- VAR(modelo_3, p = p, type = "const")
summary(VAR3)


### Inicializo las variables para luego resitringir el VAR

m <- VAR3$K # Number of variables in the VAR
T <- VAR3$obs # Number of effective sample observations, excluding "p" starting values



# Manual plotting of residuals
e <- resid(VAR3)
e <- ts(e, end = end(modelo_3), frequency = frequency(modelo_3))
colnames(e) <- c(1,2,3,4,5,6)
plot(e, main = "Residuals")


#### Analizo causalidad del modelo #######

granger_boot_modelo3 <- causality(VAR3, cause = c("ipc_sae",
                                              "imacec",
                                              "tcn",
                                              "embi_chile",
                                              "tpm_chile")
                              , boot = TRUE, boot.runs = 2000)
granger_boot_modelo3

#### No rechazo la hipótesis nula y por ende no tengo causalidad de Granger
#### dado el p-value 
constraints <- matC(m, p, 1)
VAR3a <- restrict(VAR3, method = "man", resmat = constraints)
VAR3a

#### Verifico que las raíces sean menor que 1
roots(VAR3a, modulus = TRUE)


######
# VAR Estimation (Structural Form) ####

# SVAR Estimation (AB model configuration)
SVAR3 <- SVAR(VAR3a, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR3



P <- solve(SVAR$A, SVAR$B) # inv(A) %*% B
S.SVAR <- P %*% t(P)
S.SVAR


H <- 24 # Horizon

# GENERO LAS  IRF BAJO EL MISMO NÚMERO DE PARÁMETROS

Y.boot <- boot.replicate(VAR3a, R, type)



IRF3.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot)
get.sirf.boot(IRF3.boot, m, H)


##### Ejercicio 5#####

### Inicializco el modelo VAR con las 8 variables ###

modelo_e5 <- cbind(tpm_usa,
                   cobre,
                   embi_global, 
                  imacec,
                  ipc_sae,
                  tcn,
                  embi_chile,
                  tpm_chile)

var_5 <- VARselect(modelo_e5, lag.max = 20, type = "const")
var_5
p<- var_5$selection[4] # AIC(n)
  

# El mejor modelo uno es un VAR(3) segun AIC(n)
VAR5 <- VAR(modelo_e5, p = p, type = "const")
summary(VAR5)


m <- VAR5$K # Number of variables in the VAR
T <- VAR5$obs # Number of effective sample observations, excluding "p" starting values

constraints5a <- matC(m, p, c(1,2,3))
VAR5a <- restrict(VAR5, method = "man", resmat = constraints5a)
VAR5a


constraints5b <- matC(m, p, c(2,1,3))
VAR5b <- restrict(VAR5, method = "man", resmat = constraints5b)
VAR5b


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


roots(VAR5a, modulus = TRUE)


roots(VAR5b, modulus = TRUE)





Y5a.boot <- boot.replicate(VAR5a, R, type)
SVAR5a <- SVAR(VAR5a, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR5a


IRF5a.boot <- SVAR.sirf.boot(SVAR5a, Amat, Bmat, H, gamma, Y5a.boot)
get.sirf.boot(IRF5a.boot, m, H)



Y5b.boot <- boot.replicate(VAR5b, R, type)
SVAR5b <- SVAR(VAR5b, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR5b


IRF5b.boot <- SVAR.sirf.boot(SVAR5b, Amat, Bmat, H, gamma, Y5b.boot)
get.sirf.boot(IRF5b.boot, m, H)
























