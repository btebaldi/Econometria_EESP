# ***************************************************
# Nome: Bruno Tebaldi de Queiroz Barbosa
# COD: 174887
# 
# Disciplina: Econometria 1
# Lista 1
# TA: Luiz Fantozzi Alvarez
# ***************************************************

# Limpa as variaveis anteriores
rm(list = ls())

# Carreca funcoes locais
source("./Lista 1/functions.R")

# Caracteristicas da Bernoulli
probs = c(0.1, 0.3, 0.5, 0.7, 0.9)

# declara o tmanho do Monte Carlo
n=1000

# Declara o tamanho das amostras
sizeM = c(1, 10, 100, 500, 1000, 10000)

for(p in probs) {
  # Vetor de resultados
  result = matrix( NA, nrow=n, ncol=length(sizeM))
  colnames(result) = paste(paste(c("n="), sizeM, sep=""), " e p=", p, sep="")
  
  head(result)
  
  for (i in 1:n){
    
    # Realiza um sorteio
    for(j in 1: length(sizeM))
    {
      amostra = rbinom(sizeM[j], 1, p)
      result[i,j] = mean(amostra)
    }
  }
  
  # Plotagem do histograma
  PrintGrafs(result, p)
}