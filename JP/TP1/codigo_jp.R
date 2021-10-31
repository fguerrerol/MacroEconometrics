setwd("C:/Udesa/Macro-Econometría/Git Hub/MacroEconometrics/JP/TP1")

library("readxl")
# library("tstools")
# library("xts")
# library("zoo")
# library("vars")

my_data <- read_excel("data0.xlsx")
my_data = ts(my_data, start = c(2001, 1) , end=c(2019,12), frequency = 12)

plot (my_data[,2])

### Conversión de los datos ###
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


