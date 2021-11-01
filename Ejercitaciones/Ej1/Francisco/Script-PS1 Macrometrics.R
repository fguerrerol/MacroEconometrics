#Cargamos los datos:
library("readxl")
library("tstools")
library("xts")
library("zoo")
library("vars")
setwd("/home/francisco/Documentos/MEcon/T3/MacroEconometrics/Ejercitaciones/Ej1/Series")


Data <- read_excel("data0.xlsx")

Data = ts(Data, start = c(2001, 01), frequency = 12)





#Cargamos paquetes que se usaran:
library(urca)

#Pasamos la data a formato series de tiempo:
tpm_usa <- ts(Data[,2], start=c(2001,01),end=c(2019,12), frequency=12)
p_cobre0 <- ts(Data[,3], start=c(2001,01),end=c(2019,12), frequency=12)
embi_global <- ts(Data[,4], start=c(2001,01),end=c(2019,12), frequency=12)
imacec0 <- ts(Data[,5], start=c(2001,01),end=c(2019,12), frequency=12)
ipc_sae0 <- ts(Data[,6], start=c(2001,01),end=c(2019,12), frequency=12)
tcn0 <- ts(Data[,7], start=c(2001,01),end=c(2019,12), frequency=12)
embi_chile <- ts(Data[,8], start=c(2001,01),end=c(2019,12), frequency=12)
tpm_chile <- ts(Data[,9], start=c(2001,01),end=c(2019,12), frequency=12)

#Tomamos logaritmo de las variables correspondientes:
p_cobre <- 100 * log(p_cobre0) 
imacec <- 100 * log(imacec0) 
ipc_sae <- 100 * log(ipc_sae0) 
tcn <- 100 * log(tcn0) 


#Ejercicio 2

#Graficamos las variables:
plot(tpm_usa, col = "blue", main = "TPM-USA en niveles",xlab="Fecha",ylab = "Tasa de interés externa en puntos porcentuales", ylim=c(0,7))
plot(p_cobre, col = "blue", main = "Precio Internacional del Cobre ",xlab="Fecha",ylab = "Precio Internacional del Cobre en logaritmo", ylim=c(-80,150))
plot(embi_global, col = "blue", main = "EMBI Global",xlab="Fecha",ylab = "Indice EMBI Global en puntos porcentuales", ylim=c(0,1050))
plot(imacec, col = "blue", main = "Imacec no minero, desestacionalizado",xlab="Fecha",ylab = "Imacec en logaritmo", ylim=c(350,500))
plot(ipc_sae, col = "blue", main = "IPC SAE, sin Alimentos ni Energía",xlab="Fecha",ylab = "IPC SAE en logaritmo", ylim=c(350,500))
plot(tcn, col = "blue", main = "Tipo de cambio, pesos chilenos por dolar de EE.UU",xlab="Fecha",ylab = "Tipo de Cambio en logaritmo", ylim=c(500,800))
plot(embi_chile, col = "blue", main = "Indice EMBI Chile",xlab="Fecha",ylab = "Indice EMBI Chile en puntos porcentuales", ylim=c(0,400))
plot(tpm_chile, col = "blue", main = "Tasa de Política Monetaria de Chile",xlab="Fecha",ylab = "TPM de Chile en puntos porcentuales", ylim=c(0,10))

#Incluimos en niveles o diferencias?
#Realizamos tests de Raiz Unitaria

#Augmented Dickey-Fuller:
summary(ur.df(tpm_usa,type="drift",selectlags = "BIC"))
summary(ur.df(diff(tpm_usa),type="drift",selectlags="BIC"))

summary(ur.df(p_cobre,type="trend",selectlags = "BIC"))
summary(ur.df(diff(p_cobre),type="drift",selectlags="BIC"))

summary(ur.df(embi_global,type="trend",selectlags = "BIC"))
summary(ur.df(diff(embi_global),type="drift",selectlags="BIC"))

summary(ur.df(imacec,type="trend",selectlags = "BIC"))
summary(ur.df(diff(imacec),type="drift",selectlags="BIC"))

summary(ur.df(ipc_sae,type="trend",selectlags = "BIC"))
summary(ur.df(diff(ipc_sae),type="drift",selectlags="BIC"))

summary(ur.df(tcn,type="drift",selectlags = "BIC"))
summary(ur.df(diff(tcn),type="drift",selectlags="BIC"))

summary(ur.df(embi_chile,type="trend",selectlags = "BIC"))#Estacionario en un 10%

summary(ur.df(tpm_chile,type="drift",selectlags = "BIC"))



