library("readxl")
library("tstools")
library("xts")
library("zoo")
library("vars")
setwd("/home/francisco/Documentos/MEcon/T3/MacroEconometrics/Ejercitaciones/Ej1/Series")

my_data <- read_excel("data.xlsx")

my_data = ts(my_data, start = c(2001, 01), frequency = 12)

my_data <- subset(my_data, select= -Periodo)



#### El orden de los datos está así 

### Columna 1 : Imacec No minero
### Columna 2 : IPC SAE Histórico
### Columna 3 : Tipo de cambio nominal
### Columna 4 : EMBI Chile
### Columna 5 : TASA DE POLITCA MONETARIA (PORCENTAJE)
### Columna 6 : TASA DE POLÏTICA MONETARIA EXTRANJERA (EEUU)
### Columna 7 : EMBI Global
### Columna 8 : Precio del cobre refinado.



#### Convertimos los valores 

###  Logaritmo de indice de precios al consumidor multipllicado por 100
my_data[,c(2)] <- log(my_data[,c(2)])*100

### Logarito del IMACEC no minero, desestacionalizado multiplciado por 100
my_data[,c(1)] <- log(my_data[,c(1)])*100
### Logaritmo del tipo de cambio multiplicado por 100
my_data[,c(3)] <- log(my_data[,c(3)])*100

### Logaritmo del precio de l cobre *100
my_data[,c(8)] <- log(my_data[,c(8)])*100

colnames(my_data) <- c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L", "TPM_I", "EMBI_G", "COBRE")


### Grafico 1 : Imacec No minero

plot(my_data[,c(1)], col = "black", main = "IMACEC no minero"
     ,xlab="Fecha")

### Gráfico 2 : IPC SAE Histórico

plot(my_data[,c(2)], col = "black", main = "Indice de precios al consumidor SAE"
     ,xlab="Fecha")
### Gráfico 3 : Tipo de cambio nominal

plot(my_data[,c(3)], col = "black", main = "Tipo de cambio nominal"
     ,xlab="Fecha")

### Gráfico 4 : EMBI Chile

plot(my_data[,c(4)], col = "black", main = "EMBI Chile"
     ,xlab="Fecha")

### Gráfico 5 : TASA DE POLITCA MONETARIA (PORCENTAJE)

plot(my_data[,c(5)], col = "black", main = "Tasa de política monetaria local - Chile"
     ,xlab="Fecha")

### Gráfico 6 : TASA DE POLÏTICA MONETARIA EXTRANJERA (EEUU)

plot(my_data[,c(6)], col = "black", main = "Tasa de política monetaria extranjera"
     ,xlab="Fecha",type=1)

### Gráfico 7 : EMBI Global

plot(my_data[,c(7)], col = "black", main = "EMBI Global"
     ,xlab="Fecha",type="l")

### Gráfico 8 : Precio del cobre refinado.

plot(my_data[,c(8)], col = "black", main = "Logaritmo del precio de cobre refinado"
     ,xlab="Fecha")






### Ejercicio 4
####
lag_m = 12

#### Modelo 1
#### EMBI GLOBAL VS OTRAS
modelo1 <- my_data[,-c(7:8)]

### Estimacion del lag


popt <- VARselect(modelo1, lag.max = lag_m, type = "const")

p_1 <- popt$selection[2] 


### Estimacion del primer VAR
VAR1 <- VAR(modelo1, p = p_1, type = "const") # Inclusion of exogenous variables is also possible
summary(VAR1)



m_1 <- VAR1$K # Number of variables in the VAR
T_1 <- VAR1$obs # Number of effective sample observations, excluding "p" starting values
plot(VAR1)


# Manual plotting of residuals
  e <- resid(VAR1)
  e <- ts(e, end = end(modelo1), frequency = frequency(modelo1))
  colnames(e) <- paste("e.", colnames(modelo1), sep = "")
  plot(e, main = "Residuals")

# Granger Causality ####

# GC Test, Local Vars. -> PCOM (asymptotic) 
  VAR.GC.test.asym <- causality(VAR1, cause = c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L"))
  
  VAR.GC.test.asym

# GC Test, Local Vars. -> PCOM (bootstrap)
VAR.GC.test.boot <- causality(VAR1, cause = c("IMACEC", "IPC", "TCN", "EMBI_L", "TPM_L")
                              , boot = TRUE, boot.runs = 2000)
VAR.GC.test.boot




# VAR stability

# Eigenvalues
VAR.roots <- roots(VAR1, modulus = TRUE)
VAR.roots

# Residual Serial Correlation

h.PT <- min(10, trunc(T_1 / 5)) # Rule of thumb for Portmanteau tests (Rob Hyndman) # https://robjhyndman.com/hyndsight/ljung-box-test/
# Portmanteau Test
VAR.PT.test.serial <- serial.test(VAR1, lags.pt = h.PT, type = "PT.asymptotic")
VAR.PT.test.serial

#Se tiene un p-valor de 0.067 con lo cual no se rechaza la hipótesis nula y no se tiene autocorrelacion


# Portmanteau Test (adjusted)
VAR.PT.test.serial.adj <- serial.test(VAR1, lags.pt = h.PT, type = "PT.adjusted") # Small sample correc.
VAR.PT.test.serial.adj

#### Hacinedo el test ajustado de Portmanteu vemos que no se rechaza la hipótesis nula.



h.BG <- 3
# Breusch-Godfrey Test
VAR.BG.test.serial <- serial.test(VAR1, lags.bg = h.BG, type = "BG")
VAR.BG.test.serial

# Breusch-Godfrey Test (adjusted)
VAR.BG.test.serial.adj<- serial.test(VAR1, lags.bg = h.BG, type = "ES") # Small sample correc.
VAR.BG.test.serial.adj

# Residual Normality

# Multivariate Jarque-Bera Test
VAR.JB.test <- normality.test(VAR1, multivariate.only = FALSE)
VAR.JB.test

# Residual Heteroskedasticity

q <- 3
# Multivariate ARCH Test
VAR.ARCH.test <- arch.test(VAR1, lags.multi = 12, multivariate.only = FALSE)
VAR.ARCH.test











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




