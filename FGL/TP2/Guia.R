##### Ejercitacion 2 ########
### MacroEconometría ###
### Profesor : Garcia Ciccio Javier ###
### Tutor : Libonattti Luis ####
## Integrantes ###
### Guerrero Lopez Francisco ###
### Pedregal Juan Pablo ###
### Samprón Alfredo ###

remove(list = ls(all.names = TRUE))
gc()


library("readxl")
library("tstools")
library("xts")
library("zoo")
library("vars")
library("tidyverse")


##### Ejercicio 1 ##### 

# Como primer paso, reporte el traspaso incondicional (hasta 12 meses adelante), computados con un modelo VAR
# como el visto en las clases pr ́acticas.2 Esto es, un modelo que incluya el  ́ındice de precios externos, el tipo de
# cambio nominal oficial y el IPC nivel general3 (todas en diferencias logar ́ıtmicas) identificando el shock cambiario
# como el correspondiente a la ecuación del tipo de cambio, utilizando restricciones de corto plazo recursivas. Elija
# el numero de rezagos del VAR usando criterios de informaci ́on, e imponga el supuesto de exogeneidad fuerte para
# el  ́ındice de precios internacionales. Presente tambi ́en gr ́aficos an ́alogos con un modelo que excluya el indice de
# precios externos, y compare los resultados obtenidos.



### Inicio ###


### Para iniciar construyo el Modelo VAR ###
### Este modelo incluye  las variables ####
### Indice de precios externos 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


source("PS3_LP_Tools.R")
source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")
source("PS3_SVAR_Tools.R")
source("PS3_SIRF_Transform.R")
source("PS3_SVAR_LR.R")
source("PS3_LP_Tools.R")

m2 <- read_excel("M2.xls",sheet=2)
m2 = ts(m2[-(1),2], start = c(1986, 1) , frequency = 12)



tdc_nom <- read_excel("tdc_nominal.xls")
tdc_nom = ts(tdc_nom[-(1),2], start = c(2002, 1) , frequency = 12)


ipc <- read.csv("ipc_1943_act.csv")
ipc = ts(ipc[3],start = c(1943,1),frequency = 12)

r_bruta <- read_excel("RemuneracionBruta_OEDE.xlsx")
r_bruta = ts(r_bruta[-(1),3], start = c(1995, 1) , frequency = 12)


dolar_cot <- read.csv("dolar-cot.csv")
dolar_cot = ts(dolar_cot[2], start = c(1995, 1) , frequency = 12)

emae <- read_xls("emae.xls")
emae = ts(emae[-1,3], start = c(2004, 1) , frequency = 12)

p_int <- read_excel("precios_int.xls")
p_int <- ts(p_int[2], start = c(1997, 1) , frequency = 12)


ipc <- window(ipc,start = c(2002,1))

p_int <- window(p_int,start = c(2002,1))

tdc_nom <-window(tdc_nom, start= c(2002,1))

ts.plot(ipc, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Índice de precios al consumidor") 
ts.plot(p_int, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Nivel de precios Internacionales") 
ts.plot(tdc_nom, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Tasa de cambio nominal") 


l_ipc <-log(ipc)

l_p_int <-log(p_int)

l_tdc_n <-log(tdc_nom)



ts.plot(l_ipc, type="l", lwd=2, xlab="",ylab="%",bty="n", main = " Logaritmo Índice de precios al consumidor") 
ts.plot(l_p_int, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Logaritmo de los precios  Internacionales") 
ts.plot(l_tdc_n, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Logaritmo de la tasa de cambio nominal") 

d_l_ipc  <- diff(l_ipc)
d_l_p_int <- diff(l_p_int)
d_l_tdc_n <- diff(l_tdc_n)


ts.plot(d_l_ipc, type="l", lwd=2, xlab="",ylab="%",bty="n", main = " Diff Logaritmo Índice de precios al consumidor") 
ts.plot(d_l_p_int, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Diff Logaritmo de los precios  Internacionales") 
ts.plot(d_l_tdc_n, type="l", lwd=2, xlab="",ylab="%",bty="n", main = " Diff Logaritmo de la tasa de cambio nominal") 

#### Parte 1 ####
# Generamos el modelo completo en el cual tomamos en consideración
### Subsetteeo con tal que omito el problema de los NAN, hasta lo máximo
### que se puede recortat
d_l_ipc <-window(d_l_ipc,start=c(2004,1),end=c(2019,12))

d_l_tdc_n <-window(d_l_tdc_n,start=c(2004,1),end=c(2019,12))

d_l_p_int <-window(d_l_p_int,start=c(2004,1),end=c(2019,12))

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




modelo_1 <- cbind(d_l_p_int, 
                  d_l_tdc_n,
                  d_l_ipc)

var_1 <- VARselect(modelo_1, lag.max = 15, type = "const")
var_1
p<- var_1$selection[1]
p

### Bajo el criterio de infomración de Akaike se encuentra que el
### número óptimo de rezagos para este modelo que posee 3 variables
### es igual a  3

### Generamos el modelo y obtenemos las estadísticas sumarias

VAR1 <- VAR(modelo_1, p = p, type = "const")
summary(VAR1)

m <- VAR1$K # Number of variables in the VAR
T <- VAR1$obs # Number of effective sample observations, excluding "p" starting values

### Ploteo los residuos del VAR
e <- resid(VAR1)
e <- ts(e, end = end(modelo_1), frequency = frequency(modelo_1))
colnames(e) <- c(1,2,3)
plot(e, main = "Residuals")


#### Genero esta minifuncion para restringir el feedback hacia
#### las variables locales



### Aplico la función para restringir a la matriz####
#### con la función restrict restrinjo el VAR que hize líneas atras


constraints <- matC(m, p, 2)
VAR1 <- restrict(VAR1, method = "man", resmat = constraints)
VAR1


#### Vemos que las raíces unitarias dan menor que uno por lo tanto 
#### prosigo

roots(VAR1, modulus = TRUE)



#### Genero las matrices para elaborar el VAR estructural

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


R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

### Creo y asigno el VAR estructural
SVAR_1 <- SVAR(VAR1, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
SVAR_1
source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")
source("PS2_Bootstrap.R")
source("PS2_Miscellaneous.R")

H.ERPT <- 12 # Horizon (ERPT)
Y.boot <- boot.replicate(VAR1, R, type)

ERPT_1 <- SVAR.erpt.boot(SVAR_1,Amat, Bmat, H.ERPT, 3, 2, gamma, Y.boot)


#### Aquí se grafica el primer elemento
plot(0:H.ERPT, ERPT_1$pe,
     main = "Tasa de cambio nominal pass-through a indice de precios al consumidor",
     xlab = "Horizon", ylab = "%",
     ylim = c(min(ERPT_1$lb), max(ERPT_1$ub)),
     type = "o", lwd = 2)
grid(NULL, NULL, lty = 1)
xx <- c(0:H.ERPT, H.ERPT:0)
yy <- c(c(ERPT_1$lb), rev(c(ERPT_1$ub)))
polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)
ERPT3 <- SVAR.erpt(SVAR, H.ERPT, 1, 2)
#plot.erpt3(ERPT3, H.ERPT)



####  Modelo modificaado ######

### Ahora elaboro el modelo VAR pero sin un variable ###

### Para esto genero el VAR teniendo en consideracion solo ###
 
### Este modelo se va a llamar modelo 2 y su VAR ####
### correspondiente se va a designar VAR2####

modelo_2 <- cbind(d_l_tdc_n,
                  d_l_ipc)

var_2 <- VARselect(modelo_2, lag.max = 15, type = "const")
var_2


p2<- var_2$selection[2]
p2

### El valor de p2, nos dice que los rezagos adecuados son 3
### con esto se va a proceder



### Generamos el modelo y obtenemos las estadísticas sumarias

VAR2 <- VAR(modelo_2, p = p2, type = "const")
summary(VAR2)

m <- VAR2$K # Number of variables in the VAR
T <- VAR2$obs # Number of effective sample observations, excluding "p" starting values

### Ploteo los residuos del VAR
e <- resid(VAR2)
e <- ts(e, end = end(modelo_2), frequency = frequency(modelo_2))
colnames(e) <- c(1,2)
plot(e, main = "Residuals")




### Aplico la función para restringir a la matriz####
#### con la función restrict restrinjo el VAR que hize líneas atras


constraints <- matC(m, p2, 1)
VAR2 <- restrict(VAR2, method = "man", resmat = constraints)
VAR2


#### Vemos que las raíces unitarias dan menor que uno por lo tanto 
#### prosigo

roots(VAR2, modulus = TRUE)



#### Genero las matrices para elaborar el VAR estructural

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

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")

R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

SVAR2 <- SVAR(VAR2, Amat = Amat, Bmat = Bmat, lrtest= FALSE)
Y.boot <- boot.replicate(VAR2, R, type)

ERPT_2 <- SVAR.erpt.boot(SVAR2,Amat, Bmat, H.ERPT, 2, 1, gamma, Y.boot)



plot(0:H.ERPT, ERPT_2$pe,
     main = "Tasa de cambio nominal pass-through a indice de precios al consumidor",
     xlab = "Horizon", ylab = "%",
     ylim = c(min(ERPT_2$lb), max(ERPT_2$ub)),
     type = "o", lwd = 2)
grid(NULL, NULL, lty = 1)
xx <- c(0:H.ERPT, H.ERPT:0)
yy <- c(c(ERPT_2$lb), rev(c(ERPT_2$ub)))
polygon(xx, yy, col = adjustcolor("grey", alpha.f = 0.5), border = NA)




################################


##### Ejercicio 2 #######



# 3 Variables #
modelo_1
p <- 2 # lag length
H <- 12 # Horizon
gamma <- 0.95 # Confidence level

  idx.s <- 2 # Shock: G
  idx.r <- 3 # Response: Y
  
  # Cumulative IRF
  LP.c <- lp(modelo_1, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
  plot.lp(LP.c)

########################
# 2 Variables #

p <- 3 # lag length
H <- 12 # Horizon
gamma <- 0.95 # Confidence level

idx.s <- 1 # Shock: G
idx.r <- 2 # Response: Y

# Cumulative IRF
LP.c <- lp(modelo_2, p, idx.s, idx.r, H, gamma, cumulative = TRUE)
plot.lp(LP.c)


#### Ejercicio 3 ####

r_bruta <- window(r_bruta,start =c(2004,1),end =c(2019,12))
emae <- window(emae,start = c(2004,1),end =c(2019,12))
m2 <- window(m2,start=c(2004,1,end=c(2019,12)))


ts.plot(r_bruta, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Salarios") 
ts.plot(emae, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Indice de la actividad economica") 
ts.plot(m2, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Cantidad de dinero") 


l_r_bruta <- log(r_bruta)
l_emae <- log(emae)
l_m2 <- log(m2)


ts.plot(l_r_bruta, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Salarios") 
ts.plot(l_emae, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Indice de la actividad economica") 
ts.plot(l_m2, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Cantidad de dinero") 


d_l_r_bruta <- diff(l_r_bruta)
d_l_emae <- diff(l_emae)
d_l_m2 <- diff(l_m2)


ts.plot(d_l_r_bruta, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Salarios") 
ts.plot(d_l_emae, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Indice de la actividad economica") 
ts.plot(d_l_m2, type="l", lwd=2, xlab="",ylab="%",bty="n", main = "Cantidad de dinero") 
