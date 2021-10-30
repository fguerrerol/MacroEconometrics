library("readxl")
setwd("/home/francisco/Documentos/MEcon/T3/MacroEconometrics/Ejercitaciones/Ej1/Series")
my_data <- read_excel("data.xlsx")

my_data = ts(my_data, start = c(2001, 01), frequency = 12)

my_data <- subset(my_data, select= -Periodo)
