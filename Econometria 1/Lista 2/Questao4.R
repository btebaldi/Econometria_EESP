# ***************************************************
# QUESTAO 4
# Nome: Bruno Tebaldi de Queiroz Barbosa
# COD: 174887
# 
# Disciplina: Econometria 1
# Lista 2
# TA: Luiz Fantozzi Alvarez
# ***************************************************

# Limpa as variaveis anteriores
rm(list = ls())

# define o arquivo que vai conter o output da execucao
sink(file="./Lista 2/Output/ConsoleOutput.txt")

# fixa o fator aleatorio para reproducao
seedNumber= 123456
cat(sprintf("\nFixando seed em: %d\n", seedNumber))
set.seed(seedNumber)

# Imprime a versao atual do software
cat(sprintf("\n---- VERSAO ATUAL DO SOFTWARE ----\n"))
print(version)
cat(sprintf("\n----------------------------------\n"))

# Carrega funcoes locais
source("./Lista 2/functions.R")

#  Part A
cat(sprintf("\n\nPart A\n"))

# Write a do-file that simulates 10.000 samples with 300 observations each
# of a model Yi = 10 + 0.1*Xi + ui, where Xi ~ N(2; 1) and ui ~ N(0; 1).
# Store the OLS estimate and the standard error of b1 in each simulation.
# Plot the histogram of your b1 estimates. How does it look like?

#  numero total de amostras ensaiadas
sampleTotal = 1e4
cat(sprintf("Total de amostras ensaiadas: %d\n", sampleTotal))

# Numero total de obseracoes em cada amosra
N_obs=300;
cat(sprintf("Numero de observacoes por amostra: %d\n", N_obs))

# Roda a simulacao
result = RunSimulation(sampleTotal, N_obs, 0)

# imprime os graficos
PrintGrafs(result, c("b0", "b1"), N_obs, " e erros normais")


# Part B
cat(sprintf("\n\nPart B\n"))

# Analyzing these 10.000 estimates of b1, what is the mean and the 
# standard error of b1? Are these values similar to what you would predict?

# Calcula a media dos valores
cat("--------- Media dos valores ---------\n")
print(summary(result))
cat("------------------------------------\n")

# Part C
cat(sprintf("\n\nPart C\n"))

# For each simulation, calculate a test statistic to test the hypothesis 
# that b1 = 0.1 at 5% significance level. What is the critical value?
# What is the proportion of cases that you reject the null?
# Is this what you would expect?


# Executo o teste para avalias se aceito ou rejeito 
RunT_Test(result[ ,"b1"], 0.1, result[ ,"var(b1)"], "Teste t para b1=0.1")


# Part D
cat(sprintf("\n\nPart D\n"))
# For each simulation, calculate a test statistic to test the hypothesis 
# that b1 = 0.95, b1 = 0.5 and b1 = 0 at 5% significance level. 
# What is the critical value? Do you reject the null more or less often 
# than in the previous item? Is this what you would expect?

RunT_Test(result[ ,"b1"], 0.95, result[ ,"var(b1)"], "Teste t para b1=0.95")

RunT_Test(result[ ,"b1"], 0.5, result[ ,"var(b1)"], "Teste t para b1=0.5")

RunT_Test(result[ ,"b1"], 0, result[ ,"var(b1)"], "Teste t para b1=0")



# Part E
cat(sprintf("\n\nPart E\n"))

#  declaro vetor de observacoes
vec_Obs = c(10, 30, 1000, 3000)

for (obs in vec_Obs)
{
  cat(sprintf("\n------------- %d Observacoes -------------\n", obs))
  
  cat(sprintf("\nSub Part (a)\n"))
  # Roda a simulacao
  cat(sprintf("Total de amostras ensaiadas: %d\n", sampleTotal))
  cat(sprintf("Numero de observacoes por amostra: %d\n", obs))
  result = RunSimulation(sampleTotal, obs, 0)
  
  # imprime os graficos
  PrintGrafs(result, c("b0", "b1"), obs, " e erros normais")
  
  cat(sprintf("\nSub Part (b)\n"))

  # Calcula a media dos valores
  cat("--------- Media dos valores ---------\n")
  print(summary(result))
  cat("------------------------------------\n")
  
  cat(sprintf("\nSub Part (c)\n"))
  
  # Executo o teste para avalias se aceito ou rejeito 
  RunT_Test(result[ ,"b1"], 0.1, result[ ,"var(b1)"], paste("Teste t para b1=0.1 e n_obs=", obs))
  
  
  cat(sprintf("\nSub Part (d)\n"))
  
  # Executo o teste para avalias se aceito ou rejeito 
  RunT_Test(result[ ,"b1"], 0.95, result[ ,"var(b1)"], paste("Teste t para b1=0.95 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 0.5, result[ ,"var(b1)"], paste("Teste t para b1=0.5 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 0, result[ ,"var(b1)"], paste("Teste t para b1=0 e n_obs=", obs))

  cat(sprintf("\n---- Fim do ensaio de %d Observacoes ----\n", obs))
}


# Part F
cat(sprintf("\n\nPart F\n"))

#  declaro vetor de observacoes
vec_Obs = c(300)

for (obs in vec_Obs)
{
  cat(sprintf("\n------------- %d Observacoes -------------\n", obs))
  
  cat(sprintf("\nSub Part (a)\n"))
  # Roda a simulacao
  cat(sprintf("Total de amostras ensaiadas: %d\n", sampleTotal))
  cat(sprintf("Numero de observacoes por amostra: %d\n", obs))
  result = RunSimulation(sampleTotal, obs, 2)
  
  # imprime os graficos
  PrintGrafs(result, c("b0", "b1"), obs, " e erros u=x2")
  
  cat(sprintf("\nSub Part (b)\n"))
  
  # Calcula a media dos valores
  cat("--------- Media dos valores ---------\n")
  print(summary(result))
  cat("------------------------------------\n")
  
  cat(sprintf("\nSub Part (c)\n"))
  
  # Executo o teste para avalias se aceito ou rejeito 
  RunT_Test(result[ ,"b1"], 0.1, result[ ,"var(b1)"], paste("Teste t para b1=0.1 e n_obs=", obs))
  
  
  cat(sprintf("\nSub Part (d)\n"))
  
  # Executo o teste para avalias se aceito ou rejeito 
  RunT_Test(result[ ,"b1"], 0.95, result[ ,"var(b1)"], paste("Teste t para b1=0.95 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 0.5, result[ ,"var(b1)"], paste("Teste t para b1=0.5 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 0, result[ ,"var(b1)"], paste("Teste t para b1=0 e n_obs=", obs))
  
  cat(sprintf("\n---- Fim do ensaio de %d Observacoes ----\n", obs))
}



# Part G
cat(sprintf("\n\nPart G\n"))

#  declaro vetor de observacoes
vec_Obs = c(300)

for (obs in vec_Obs)
{
  cat(sprintf("\n------------- %d Observacoes -------------\n", obs))
  
  cat(sprintf("\nSub Part (a)\n"))
  # Roda a simulacao
  cat(sprintf("Total de amostras ensaiadas: %d\n", sampleTotal))
  cat(sprintf("Numero de observacoes por amostra: %d\n", obs))
  result = RunSimulation(sampleTotal, obs, 3)
  
  # imprime os graficos
  PrintGrafs(result, c("b0", "b1"), obs, " e erros u=-2_x_ei")
  
  cat(sprintf("\nSub Part (b)\n"))
  
  # Calcula a media dos valores
  cat("--------- Media dos valores ---------\n")
  print(summary(result))
  cat("------------------------------------\n")
  
  cat(sprintf("\nSub Part (c)\n"))
  
  # Executo o teste para avalias se aceito ou rejeito 
  RunT_Test(result[ ,"b1"], 0.1, result[ ,"var(b1)"], paste("Teste t para b1=0.1 e n_obs=", obs))
  
  
  cat(sprintf("\nSub Part (d)\n"))
  
  # Executo o teste para avalias se aceito ou rejeito 
  RunT_Test(result[ ,"b1"], 0.95, result[ ,"var(b1)"], paste("Teste t para b1=0.95 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 0.5, result[ ,"var(b1)"], paste("Teste t para b1=0.5 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 0, result[ ,"var(b1)"], paste("Teste t para b1=0 e n_obs=", obs))
  RunT_Test(result[ ,"b1"], 1.1, result[ ,"var(b1)"], paste("Teste t para b1=1.1 e n_obs=", obs))
  
  cat(sprintf("\n---- Fim do ensaio de %d Observacoes ----\n", obs))
}



# fecha o arquivo com o output da execucao 
sink()

