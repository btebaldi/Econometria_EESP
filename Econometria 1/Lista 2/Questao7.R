# ***************************************************
# QUESTAO 7
# Nome: Bruno Tebaldi de Queiroz Barbosa
# COD: 174887
# 
# Disciplina: Econometria 1
# Lista 1
# TA: Luiz Fantozzi Alvarez
# ***************************************************

# Limpa as variaveis anteriores
rm(list = ls()[ls()!="base_clima_datasus2010"])

# define o arquivo que vai conter o output da execucao
# sink(file="./Lista 2/Output/ConsoleOutput.txt")

# Carrega bibliotecas externas
library(haven)
library(MASS)

# base_clima_datasus2010 <- read_dta("C:/Users/Tebaldi/Downloads/Soluções_PF2017_Q1e2/1-s2.0-S0304387814001096-mmc1/base_clima_datasus2010.dta")
# View(base_clima_datasus2010)
colnames(base_clima_datasus2010)

a= unique(base_clima_datasus2010[,"código_6"])
b=unique(base_clima_datasus2010[,c("ano", "mes")])

# unique(base_clima_datasus2010[,c("ano", "mes")])

dim(a)
dim(b)

# head(base_clima_datasus2010[,"dyang_last12"])

mdl = lm("k_obt_nasc ~ dyang_last12", data = base_clima_datasus2010)
summary(mdl)

# aumento de 1% da chuva em relacao a sua media historica esta associado em media 
# 0.01 * -6.1571 queda em mortes por cem mil habitantes
f = formula("k_obt_nasc ~ dyang_last12")
mdl2 = rlm(formula = f, data = base_clima_datasus2010)

summary(mdl2)

mdl3 = lm("k_obt_nasc ~ dyang_last12 + temp_mean12", data = base_clima_datasus2010)
summary(mdl3)

