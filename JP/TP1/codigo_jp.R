setwd("C:/Udesa/Macro-Econometr�a/Git Hub/MacroEconometrics/JP/TP1")

library("readxl")
# library("tstools")
# library("xts")
# library("zoo")
# library("vars")

df <- read_excel("data0.xlsx")
my_data = ts(df, start = c(2001, 1) , end=c(2019,12), frequency = 12)

plot (my_data[,2])

### Conversi�n de los datos ###
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

#### Punto 3 ####

reg <- lm( diff(my_data[,c(5)]) ~  diff((my_data[,c(10)])) +
             diff((my_data[,c(11)])) + diff((my_data[,c(12)])))
imacec_limpio <- ts(reg$residuals, start=c(2001,02),end=c(2019,12), frequency=12)
descarte <- ts(reg[["fitted.values"]], start=c(2001,02),end=c(2019,12), frequency=12)

par(mfrow=c(3,1))
ts.plot(diff(my_data[,c(5)]), type="l", lwd=2, xlab="",ylab="",bty="n", main = "Imacec no minero en ln por 100") 
ts.plot(imacec_limpio, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Residuos - Imacec no minero limpio") 
ts.plot(descarte, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Terremoto y conflicto social sobre Imacec no minero") 

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
new_imac2 <- reg2$residuals
new_imac2 <- ts(new_imac2, start=c(2001,02), frequency = 12)

par(mfrow=c(3,1))
ts.plot(new_imac2, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Ellos") 
ts.plot(imacec_limpio, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Nuestro") 
ts.plot(imacec_limpio - new_imac2 , type="l", lwd=2, xlab="",ylab="",bty="n", main = "resta") 

# Para el documento 
descarte <- ts(reg2[["fitted.values"]], start=c(2001,02),end=c(2019,12), frequency=12)


par(mfrow=c(3,1))
ts.plot(diff(my_data[,c(5)]), type="l", lwd=2, xlab="",ylab="",bty="n", main = "Imacec no minero en ln por 100") 
ts.plot(new_imac2, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Residuos - Imacec no minero limpio") 
ts.plot(descarte, type="l", lwd=2, xlab="",ylab="",bty="n", main = "Terremoto y conflicto social sobre Imacec no minero") 


#### punto 4 ####

# Modelo 1 : la internacional es TPM_USA

modelo_1 <- cbind(diff(my_data[,c(2)]), 
                  new_imac2,
                  diff(my_data[,c(6)]), diff(my_data[,c(7)]),
                  diff(my_data[,c(8)]), diff(my_data[,c(9)]))

library(vars)
var_1 <- VARselect(modelo_1, lag.max = 15, type = "const")
var_1
p_1 <- var_1$selection[1] # AIC(n)

# El mejor modelo uno es un VAR(3) segun AIC(n)
VAR1 <- VAR(modelo_1, p = p_1, type = "const")
summary(VAR1)

# Manual plotting of residuals
e <- resid(VAR1)
e <- ts(e, end = end(modelo_1), frequency = frequency(modelo_1))
colnames(e) <- c(1,2,3,4,5,6)
plot(e, main = "Residuals")

# VAR stability

# Eigenvalues son menores a 1 - entonces el VAR es estable
VAR1.roots <- roots(VAR1, modulus = TRUE)
VAR1.roots

T_1 <- VAR1$obs
# Residual Serial Correlation
h.PT <- min(10, trunc(T_1 / 5)) # Rule of thumb for Portmanteau tests (Rob Hyndman) # https://robjhyndman.com/hyndsight/ljung-box-test/
# Portmanteau Test
VAR1.PT.test.serial <- serial.test(VAR1, lags.pt = h.PT, type = "PT.asymptotic")
VAR1.PT.test.serial
# rechazamos la hip�tesis nula de que los errores no est�n autocorrelacionados
# osea que hay un problema de autocorrelacion de los errores 

# Portmanteau Test (adjusted)
VAR1.PT.test.serial.adj <- serial.test(VAR1, lags.pt = h.PT, type = "PT.adjusted") # Small sample correc.
VAR1.PT.test.serial.adj
# lo re rechaza

# Test de normalidad de los residuos: rechazado
VAR1.JB.test <- normality.test(VAR1, multivariate.only = FALSE)
VAR1.JB.test


