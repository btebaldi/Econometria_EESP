# ***************************************************
# Nome: Bruno Tebaldi de Queiroz Barbosa
# COD: 174887
# 
# Econometria 1
# William H. Greene - Econometric Analysis, 7th Edition (2011, Prentice Hall)
# 8.3.4 TWO-STAGE LEAST SQUARES
# Overidentification
# ***************************************************

# Limpa as variaveis anteriores
rm(list = ls())

# Carrega bibliotecas
library(readxl)
library(AER)

# Carrega base de dados
WAGES = read_excel("Lista 3/WAGES.xls")

# adiciona coluna de 1's
WAGES[,"1s"] = 1
colnames(WAGES)

# Formula normal sugerida
f = formula("LWAGE ~ EXP + I(EXP^2) + WKS + OCC + IND + SOUTH + SMSA + MS + UNION + ED + FEM + BLK")
mdl = lm (f, WAGES)
summary(mdl)

# Formula OLS1
f = formula("WKS ~ LWAGE + ED + UNION + FEM")
mdl = lm (f, WAGES)
summary(mdl)

# Definicao da variavel instrumental Z1 = [1, IND, ED, UNION, FEM)]
Z1_cols = c("1s", "IND", "ED", "UNION", "FEM")
X_cols = c("1s", "LWAGE", "ED", "UNION", "FEM")

# define o vetor de instrumentos
Z1 = as.matrix(WAGES[,Z1_cols])
head(Z1)

# define o vetor que sera os regressores
X = as.matrix(WAGES[,X_cols])
head(X)

# define o vetor de variavel depente
Y = as.matrix(WAGES[,"WKS"])
head(Y)


# Calculo do beta IV com um unico instrumento

# b_IV = (Z'X)_1 Z'Y
b_iv = solve(t(Z1)%*%X) %*% t(Z1) %*% Y

# sigma2 = 1/n sum (y − xb_IV)^2
res = Y - X %*% b_iv
sigma2 = (t(res) %*% res) / length(res)

# var_bIV = sigma2 (Z'X)_1 (Z'Z) (X'Z)_1  
var = as.numeric(sigma2) * solve(t(Z1)%*%X) %*% t(Z1) %*% Z1 %*% solve(t(X) %*% Z1)
print(data.frame(b_iv, "s.e" = diag(var)^0.5))

# Calculo do IV via pacote
iv1 = ivreg(WKS ~ LWAGE + ED + UNION + FEM | IND + ED + UNION + FEM, data = WAGES)
summary(iv1)

# Calculo do IV via 2SLS
stg1_mdl_z1 = lm(LWAGE ~ IND + ED + UNION + FEM, data=WAGES)
X_hat_2sls_z1 = as.matrix(stg1_mdl_z1$fitted.values)

stg2_mdl_z1 = lm(WKS ~ X_hat_2sls_z1 + ED + UNION + FEM, data=cbind(WAGES, X_hat_2sls_z1))
summary(stg2_mdl_z1)

# Calculo do residuo
res_2sls_z1 = Y - X %*% stg2_mdl_z1$coefficients
sigma2_2sls_z1 = (t(res_2sls_z1) %*% res_2sls_z1) / length(res_2sls_z1)

# Maneira errada de se calcular a VAR
Sxx_2stg_z1 = t(as.matrix(WAGES[, c("1s", "LWAGE", "ED", "UNION", "FEM")])) %*% as.matrix(WAGES[, c("1s", "LWAGE", "ED", "UNION", "FEM")])
var_2stg_z1 = as.numeric(sigma2_2sls_z1) * solve(Sxx_2stg_z1)
print(data.frame(stg2_mdl_z1$coefficients, "s.e" = diag(var_2stg_z1)^0.5))


# Maneira correta de se calcular a VAR
# Defino o vetor de instrumentos "sintetico"
Z1_2sls = as.matrix(cbind(WAGES, X_hat_2sls_z1))[, c("1s", "X_hat_2sls_z1", "ED", "UNION", "FEM")]

var_2stg_z1_correto = as.numeric(sigma2_2sls_z1) * solve(t(Z1_2sls)%*%X) %*% t(Z1_2sls) %*% Z1_2sls %*% solve(t(X) %*% Z1_2sls)
print(data.frame(stg2_mdl_z1$coefficients, "s.e" = diag(var_2stg_z1_correto)^0.5))


# Definicao da variavel instrumental Z2 = [1, IND, ED, UNION, FEM, SMSA].
Z2_cols = c("1s", "IND", "ED", "UNION", "FEM", "SMSA")
Z2 = as.matrix(WAGES[,Z2_cols])

# como temos mais de um instrumento vamos usar a metodologia de 2SLS para calcular 
# um instrumento que eh combinacao dos demais
X_z2_hat = Z2%*%solve(t(Z2)%*%Z2)%*%t(Z2)%*%X

# b_IV = (Z'X)_1 Z'Y
b_iv_z2 = solve(t(X_z2_hat)%*%X) %*% t(X_z2_hat) %*% Y

# sigma2 = 1/n sum (y − xb_IV)^2
res_z2 = Y - X %*% b_iv_z2
sigma2_z2 = (t(res_z2) %*% res_z2) / length(res_z2)

# var_bIV = sigma2 (Z'X)_1 (Z'Z) (X'Z)_1  
var_z2 = as.numeric(sigma2_z2) * solve(t(X_z2_hat)%*%X) %*% t(X_z2_hat) %*% X_z2_hat %*% solve(t(X) %*% X_z2_hat)
print(data.frame(b_iv_z2, "s.e" = diag(var_z2)^0.5))


# Resolucao via 2SLS
# Realizo o 1 estagio, regredindo LWAGE contra todos os intrumentos + controle + constante
stg1_mdl_z2 = lm(LWAGE ~ IND + ED + UNION + FEM + SMSA, data=WAGES)
X_hat_2sls_z2 = as.matrix(stg1_mdl_z2$fitted.values)

#  realizo o 2 estagio, regredindo a dependente contra constante + X_hat + controles 
stg2_mdl_z2 = lm(WKS ~ X_hat_2sls_z2 + ED + UNION + FEM, data=cbind(WAGES, X_hat_2sls_z2))
summary(stg2_mdl_z2)

# defino o vetor de instrumentos "sintetico"
Z2_2sls = as.matrix(cbind(WAGES[,c("1s", "ED", "UNION", "FEM")], X_hat_2sls_z2))

# calculo o residuo real da regressao 
res_2sls_z2 = Y - X %*% stg2_mdl_z2$coefficients
sigma2_2sls_z2 = (t(res_2sls_z2) %*% res_2sls_z2) / length(res_2sls_z2)

# calculo a variancia com a formula usual, contudo utilizando o vetor de instrumentos "sintetico"
var_2stg_z2_correto = as.numeric(sigma2_2sls_z2) * solve(t(Z2_2sls)%*%X) %*% t(Z2_2sls) %*% Z2_2sls %*% solve(t(X) %*% Z2_2sls)
print(data.frame(stg2_mdl_z2$coefficients, "s.e" = diag(var_2stg_z2_correto)^0.5))


#  resolucao via pacote R
iv2 = ivreg(WKS ~ LWAGE + ED + UNION + FEM | IND + ED + UNION + FEM + SMSA, data = WAGES)
summary(iv2)
