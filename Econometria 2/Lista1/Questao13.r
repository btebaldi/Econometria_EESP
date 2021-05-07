###############################################################################
# Class: Econometrics II
# Professor: Cristine Campos de Xavier Pinto
# TA: Luis Alvarez
# Code by: Bruno Tebaldi de Queiroz Barbosa
#
# Topic: Lista 1 - Questao 13
###############################################################################

# Clear all past variables
rm(list = ls())

# Load the data base
library(readr)
library(dplyr)
library(sandwich)
library(quantreg)

# Carrega base de dados
consumerData = read_csv("Database/consumer.csv")

filterData = select(consumerData, PX1, RATEX, UNEMP, AGE, SEX, FAMSIZE, INCOME)

# Questao a)

mdl.ols = lm(PX1 ~ RATEX + UNEMP + AGE + SEX + FAMSIZE + INCOME, data = filterData)
summary(mdl.ols)

# Questao b)

#Running linear model for \tau \in \{0.25,0.5, 0.75\}
mdl.quant = rq(PX1 ~ RATEX + UNEMP + AGE + SEX + FAMSIZE + INCOME, data = consumerData, tau = c(0.25,0.5,0.75))

summary(mdl.quant, se = "nid")
