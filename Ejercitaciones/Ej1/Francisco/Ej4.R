library("readxl")
library("xts")
library("zoo")
library("vars")
setwd("/home/francisco/Documentos/MEcon/T3/MacroEconometrics/Ejercitaciones/Ej1/Series")

library("readxl")
# library("tstools")
# library("xts")
# library("zoo")
# library("vars")

my_data <- read_excel("data0.xlsx")
my_data = ts(my_data, start = c(2001, 1) , end=c(2019,12), frequency = 12)

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

#### Puntos 2 ####
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




#Locales
ipc_sae         <- ts(diff(my_data[,c(6)]))

tcn_var         <- ts(diff(my_data[,c(7)]))

imacec_var      <- ts(diff(my_data[,c(5)]))

ltpm_var        <- ts(diff(my_data[,c(9)]))

embi_local_var  <- ts(diff(my_data[,c(8)]))

#Internacionales

itpm_var         <- ts(diff(my_data[,c(2)]))

cobre_var        <- ts(diff(my_data[,c(3)]))

embi_global_var  <- ts(diff(my_data[,c(4)]))



### Ejercicio 4
####
lag_m = 12


####### Modelos VAR



modelo1 <- cbind(itpm_var,
                 ipc_sae,
                 tcn_var,
                 imacec_var,
                 ltpm_var,
                 embi_local_var)

modelo2 <- cbind(cobre_var,
                 ipc_sae,
                 tcn_var,
                 imacec_var,
                 ltpm_var,
                 embi_local_var)


modelo3 <- cbind(embi_global_var,
                 ipc_sae,
                 tcn_var,
                 imacec_var,
                 ltpm_var,
                 embi_local_var)

popt <- VARselect(modelo1, lag.max = lag_m, type = "const")

p_1 <- popt$selection[2] 

VAR1 <- VAR(modelo1, p = p_1, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR1)


IRF1 <- irf(VAR1, impulse="itpm_var",
            response= c("embi_local_var","ipc_sae",
                        "tcn_var","ltpm_var","imacec_var"),
            boot=FALSE, runs=100)
plot(IRF1)



### Estimacion del primer VAR

popt <- VARselect(modelo2, lag.max = lag_m, type = "const")

p_2 <- popt$selection[2] 



VAR2 <- VAR(modelo2, p = p_2, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR2)

IRF2 <- irf(VAR2, impulse="cobre_var",
            response= c("embi_local_var","ipc_sae",
                        "tcn_var","ltpm_var","imacec_var"),
            boot=FALSE, runs=100)
plot(IRF2)

### Estimacion del primer VAR

popt <- VARselect(modelo3, lag.max = lag_m, type = "const")

p_3 <- popt$selection[2] 


VAR3 <- VAR(modelo3, p = p_3, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR3)


IRF3 <- irf(VAR3, impulse="embi_global_var",
            response= c("embi_local_var","ipc_sae",
                        "tcn_var","ltpm_var","imacec_var"),
            boot=FALSE, runs=100)
plot(IRF3)






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
  Cm
}











#### Modelo 2
#### PRECIO COBRE VS OTRAS
modelo2 <- my_data[,-c(6:7)]

### Estimacion del lag

m2_pmax <- 12 # Maximum lag order

popt <- VARselect(modelo2, lag.max = lag_m, type = "const")

p_2 <- popt$selection[2] 


### Estimacion del segundo VAR
VAR2 <- VAR(modelo2, p = p_2, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR2)



m_1 <- VAR2$K # Number of variables in the VAR
T_1 <- VAR2$obs # Number of effective sample observations, excluding "p" starting values
plot(VAR2)


# Manual plotting of residuals
e <- resid(VAR2)
e <- ts(e, end = end(modelo2), frequency = frequency(modelo2))
colnames(e) <- paste("e.", colnames(modelo1), sep = "")
plot(e, main = "Residuals")

# Granger Causality ####

# GC Test, Local Vars. -> PCOM (asymptotic) 
VAR.GC.test.asym <- causality(VAR2, cause = c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L"))

VAR.GC.test.asym

# GC Test, Local Vars. -> PCOM (bootstrap)
VAR.GC.test.boot <- causality(VAR2, cause = c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L")
                              , boot = TRUE, boot.runs = 2000)
VAR.GC.test.boot





#### Modelo 3
#### TPM EEUU
modelo3 <- my_data[,-c(6,8)]


popt <- VARselect(modelo3, lag.max = lag_m, type = "const")

p_3 <- popt$selection[2] 


### Estimacion del segundo VAR
VAR3 <- VAR(modelo3, p = p_3, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR3)




m_3 <- VAR3$K # Number of variables in the VAR
T_3 <- VAR3$obs # Number of effective sample observations, excluding "p" starting values
plot(VAR3)


# Manual plotting of residuals
e <- resid(VAR3)
e <- ts(e, end = end(modelo3), frequency = frequency(modelo3))
colnames(e) <- paste("e.", colnames(modelo3), sep = "")
plot(e, main = "Residuals")

# Granger Causality ####

# GC Test, Local Vars. -> PCOM (asymptotic) 
VAR.GC.test.asym <- causality(VAR3, cause = c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L"))

VAR.GC.test.asym

# GC Test, Local Vars. -> PCOM (bootstrap)
VAR.GC.test.boot <- causality(VAR3, cause = c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L")
                              , boot = TRUE, boot.runs = 2000)
VAR.GC.test.boot




