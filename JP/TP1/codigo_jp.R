setwd("C:/Udesa/Macro-Econometría/Git Hub/MacroEconometrics/JP/TP1")

library("readxl")
library("tstools")
library("xts")
library("zoo")
library("vars")

my_data <- read_excel("data0.xlsx")
my_data = ts(my_data, start = c(2001, 01), frequency = 12)
plot (my_data[,2])



