#Cargamos los datos:
library(readxl)
Data <- read_excel("Desktop/Data.xlsx")
View(Data)

#Cargamos paquetes que se usaran:
library(urca)
library(vars)

#Pasamos la data a formato series de tiempo:
tpm_usa <- ts(Data[,2], start=c(2001,01),end=c(2019,12), frequency=12) #Tendencia
p_cobre0 <- ts(Data[,3], start=c(2001,01),end=c(2019,12), frequency=12)#Tendencia
embi_global <- ts(Data[,4], start=c(2001,01),end=c(2019,12), frequency=12)#Tendencia
imacec0 <- ts(Data[,5], start=c(2001,01),end=c(2019,12), frequency=12)#Tendencia
ipc_sae0 <- ts(Data[,6], start=c(2001,01),end=c(2019,12), frequency=12)#Tendencia
tcn0 <- ts(Data[,7], start=c(2001,01),end=c(2019,12), frequency=12)#No tendencia
embi_chile <- ts(Data[,8], start=c(2001,01),end=c(2019,12), frequency=12)#Tendencia
tpm_chile <- ts(Data[,9], start=c(2001,01),end=c(2019,12), frequency=12)#No tendencia

#Tomamos logaritmo de las variables correspondientes:
p_cobre <- 100 * log(p_cobre0) 
imacec <- 100 * log(imacec0) 
ipc_sae <- 100 * log(ipc_sae0) 
tcn <- 100 * log(tcn0) 


####################### Ejercicio 2 ####################
plot(p_cobre0, col = "blue", main = "Precio Internacional del Cobre sin log ",xlab="Fecha",ylab = "Precio Internacional del Cobre", ylim=c(0,5))

#Graficamos las variables:
plot(tpm_usa, col = "blue", main = "TPM-USA en niveles",xlab="Fecha",ylab = "Tasa de interés externa en puntos porcentuales", ylim=c(0,7))
plot(p_cobre, col = "blue", main = "Precio Internacional del Cobre con log ",xlab="Fecha",ylab = "Precio Internacional del Cobre en logaritmo", ylim=c(-80,150))
plot(embi_global, col = "blue", main = "EMBI Global",xlab="Fecha",ylab = "Indice EMBI Global en puntos porcentuales", ylim=c(0,1050))
plot(imacec, col = "blue", main = "Imacec no minero, desestacionalizado",xlab="Fecha",ylab = "Imacec en logaritmo", ylim=c(350,500))
plot(ipc_sae, col = "blue", main = "IPC SAE, sin Alimentos ni Energía",xlab="Fecha",ylab = "IPC SAE en logaritmo", ylim=c(350,500))
plot(tcn, col = "blue", main = "Tipo de cambio, pesos chilenos por dolar de EE.UU",xlab="Fecha",ylab = "Tipo de Cambio en logaritmo", ylim=c(500,800))
plot(embi_chile, col = "blue", main = "Indice EMBI Chile",xlab="Fecha",ylab = "Indice EMBI Chile en puntos porcentuales", ylim=c(0,400))
plot(tpm_chile, col = "blue", main = "Tasa de Política Monetaria de Chile",xlab="Fecha",ylab = "TPM de Chile en puntos porcentuales", ylim=c(0,10))

#Incluimos en niveles o diferencias?
#Realizamos tests de Raiz Unitaria

################Augmented Dickey-Fuller:
summary(ur.df(tpm_usa,type="trend",selectlags = "BIC"))
summary(ur.df(diff(tpm_usa),type="drift",selectlags="BIC"))#I(1)

summary(ur.df(p_cobre,type="trend",selectlags = "BIC"))
summary(ur.df(diff(p_cobre),type="drift",selectlags="BIC"))#I(1)

summary(ur.df(embi_global,type="drift",selectlags = "BIC"))#I(0)

summary(ur.df(imacec,type="trend",selectlags = "BIC"))
summary(ur.df(diff(imacec),type="drift",selectlags="BIC"))#I(1)

summary(ur.df(ipc_sae,type="trend",selectlags = "BIC"))
summary(ur.df(diff(ipc_sae),type="drift",selectlags="BIC"))#I(1)

summary(ur.df(tcn,type="drift",selectlags = "BIC"))
summary(ur.df(diff(tcn),type="drift",selectlags="BIC"))#I(1)

summary(ur.df(embi_chile,type="drift",selectlags = "BIC"))#I(0)

summary(ur.df(tpm_chile,type="drift",selectlags = "BIC"))#I(0)


############PP test:

summary(ur.pp(tpm_usa, type="Z-tau", model="trend",lags="long"))
summary(ur.pp(diff(tpm_usa, type="Z-tau", model="constant",lags="long")))#I(1)

summary(ur.pp(p_cobre, type="Z-tau", model="trend",lags="long"))
summary(ur.pp(diff(p_cobre, type="Z-tau", model="constant",lags="long")))#I(1)

summary(ur.pp(embi_global, type="Z-tau", model="constant",lags="long"))
summary(ur.pp(diff(embi_global, type="Z-tau", model="constant",lags="long")))#I(1)

summary(ur.pp(imacec, type="Z-tau", model="trend",lags="long"))
summary(ur.pp(diff(imacec, type="Z-tau", model="constant",lags="long")))#I(1)

summary(ur.pp(ipc_sae, type="Z-tau", model="trend",lags="long"))
summary(ur.pp(diff(ipc_sae, type="Z-tau", model="constant",lags="long")))#(1)

summary(ur.pp(tcn, type="Z-tau", model="constant",lags="long"))
summary(ur.pp(diff(tcn, type="Z-tau", model="constant",lags="long")))#I(1)

summary(ur.pp(embi_chile, type="Z-tau", model="constant",lags="long"))#I(0)

summary(ur.pp(tpm_chile, type="Z-tau", model="constant",lags="long"))#I(0)


############KPSS test:

summary(ur.kpss(tpm_usa,type="tau",lags="long")) #I(1)
summary(ur.kpss(diff(tpm_usa),type="mu",lags="long"))

summary(ur.kpss(p_cobre,type="tau",lags="long")) #I(1)
summary(ur.kpss(diff(p_cobre),type="mu",lags="long"))

summary(ur.kpss(embi_global,type="mu",lags="long")) #I(1)
summary(ur.kpss(diff(embi_global),type="mu",lags="long"))

summary(ur.kpss(imacec,type="tau",lags="long"))
summary(ur.kpss(diff(imacec),type="mu",lags="long"))#I(1)

summary(ur.kpss(ipc_sae,type="tau",lags="long"))
summary(ur.kpss(diff(ipc_sae),type="mu",lags="long")) #I(1)

summary(ur.kpss(tcn,type="mu",lags="long")) #I(0)

summary(ur.kpss(embi_chile,type="mu",lags="long")) #I(0)

summary(ur.kpss(tpm_chile,type="mu",lags="long")) #I(0)

#Conclusion de los 3 tests:
#tpm_usa es I(1)
#p_cobre es I(1)
#embi_global es I(1)
#imacec es I(1)
#ipc_sae es I(1)
#tcn es I(1)
#embi_chile es I(0)
#tpm_chile es I(0)
#Por lo tanto, presentamos solo en diferencias al embi chile y a la tasa de politica monetaria de chile, las demas en diferencias.

#Procedemos a diferenciar las correspondientes:
dif_tpm_usa <- diff(tpm_usa)
dif_p_cobre <- diff(p_cobre)
dif_embi_global <- diff(embi_global)
dif_imacec <- diff(imacec)
dif_ipc_sae <- diff(ipc_sae)
dif_tcn <- diff(tcn)

#Graficamos:
plot(dif_tpm_usa, col = "blue", main = "TPM-USA en niveles",xlab="Fecha",ylab = "Tasa de interés externa en puntos porcentuales", ylim=c(-1,1))
plot(dif_p_cobre, col = "blue", main = "Precio Internacional del Cobre con log ",xlab="Fecha",ylab = "Precio Internacional del Cobre en logaritmo", ylim=c(-40,40))
plot(dif_embi_global, col = "blue", main = "EMBI Global",xlab="Fecha",ylab = "Indice EMBI Global en puntos porcentuales", ylim=c(-250,300))
plot(dif_imacec, col = "blue", main = "Imacec no minero, desestacionalizado",xlab="Fecha",ylab = "Imacec en logaritmo", ylim=c(-7,7))
plot(dif_ipc_sae, col = "blue", main = "IPC SAE, sin Alimentos ni Energía",xlab="Fecha",ylab = "IPC SAE en logaritmo", ylim=c(-1,1))
plot(dif_tcn, col = "blue", main = "Tipo de cambio, pesos chilenos por dolar de EE.UU",xlab="Fecha",ylab = "Tipo de Cambio en logaritmo", ylim=c(-10,20))

######################Ejercicio 3######################3

#Primero creamos nuestra variable dummy que vale 1 para Marzo 2010, Octubre y Noviembre 2019:
Dummy <- ts(Data[,10],start=c(2001,01),end=c(2019,12),frequency=12)

imacec_reg <- lm(imacec~Dummy)
#Tomo sus residuos
imacec_reg$residuals
#Lo paso a variable ts:
imacec_residuals <- ts(imacec_reg$residuals, start=c(2001,01),end=c(2019,12), frequency=12)
plot(imacec_residuals)













