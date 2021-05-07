# ***************************************************
# Lista 3 - QUESTAO 5
# Nome: Bruno Tebaldi de Queiroz Barbosa
# COD: 174887
# 
# Disciplina: Econometria 1
# TA: Luiz Fantozzi Alvarez
# ***************************************************

# Limpa as variaveis anteriores
rm(list = ls())

# carrega bibliotecas externas
library(haven)
library(sandwich)
library(AER)

# Importa base de dados
JEC = read_dta("./Lista 3/JEC.dta")

# acrescento uma coluna de 1's no banco de dados (constante de regressao)
JEC[, "1s"] = 1 

# cria as variaveis Log(P) e Log(Q) 
JEC[,"ln_P"] = log(JEC[,"price"])
JEC[,"ln_Q"] = log(JEC[,"quantity"])

  # Seleciona as variaveis de controle
  seascols = colnames(JEC)[grep("seas.",x = colnames(JEC), perl = T)]
  
  # estima o OLS pooled
  f1 = formula(paste("ln_Q ~ 1 + ln_P + ", paste(seascols, collapse = " + "), collapse = ""))
  mdl_1 = lm(f1, JEC)

  print(summary(mdl_1))
  
  robust_errors_mdl_1 = diag(sandwich::vcovHC(mdl_1, "HC1"))^0.5
  
  cat(sprintf("Erro robusto ln_P: %f\n", robust_errors_mdl_1["ln_P"]))

  # Calculo do erro robusto (igual ao stata)
  res = mdl_1$residuals
  
  # defino a matrix de regressores e valores
  X = mdl_1$model
  X[,1] = 1
  X = as.matrix(X)
  colnames(X)[1] = "Const"
  
  # Calculo a matrix de ressiduos de White 
  # OBS: nao temos correlacao entao sera uma matriz diagonal com os erros ao quadrado corrigido pelos DF
  omega = diag(res^2 * length(res)/(nrow(X) - ncol(X)))
  
  # calcula a matrix de variancia e covariancia.
  vcov = solve(crossprod(X)) %*% t(X) %*% omega %*% X %*% solve(crossprod(X))
  diag(vcov)^0.5
  

# Utiliza cartel como variavel instrumental do preco

# definicao do innstrumento
Z_cartel_cols = c("1s", "cartel", seascols)
Z_cartel = as.matrix(JEC[,Z_cartel_cols]) 

# definicao da matrix X
X_cols = c("1s", "ln_P", seascols)
X = as.matrix(JEC[,X_cols]) 

# definicao da matrix Y
Y_cols = c("ln_Q")
Y = as.matrix(JEC[,Y_cols]) 

# head(Z_cartel)
  
# Calculo do estimador
b_iv = solve(t(Z_cartel)%*%X) %*% t(Z_cartel)%*%Y

res = Y - X%*%b_iv
sigma2 = t(res)%*%res/(length(res)-length(b_iv))
var = as.numeric(sigma2) * solve(t(Z_cartel)%*%X) %*% t(Z_cartel) %*% Z_cartel %*% solve(t(X) %*% Z_cartel)
t_stat = b_iv/diag(var)^0.5
p_value = 2*pt(abs(t_stat), length(res)-length(b_iv), lower.tail = F, log.p = FALSE)

result = data.frame(b_iv, "s.e." = diag(var)^0.5, "t-stat" = t_stat, "p.value" = p_value)
colnames(result) = c("Coeff", "s.e.", "t-stat", "p-value")
print (result)

# teste se Beta = -1
t = (b_iv["ln_P",] + 1)/(diag(var)["ln_P"])^0.5
pvalue = 2*pnorm(abs(t), mean = 0, sd = 1, lower.tail = F, log.p = FALSE)

cat(sprintf("\nTeste de beta = -1\nt-stat: %f\tp-value: %f\n", t, pvalue))

#  Estimando utilizando o Pacote AER
cat("Estimando utilizando o Pacote AER")
iv_mdl1 = ivreg(ln_Q ~ ln_P + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | cartel + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = JEC)
print(summary(iv_mdl1))
