PrintGrafs = function(x, cols, N_obs, observacao)
{
  for(i in 1:length(cols))
  {
    # Define o nome do arquivo
    FileName = paste("./Lista 2/Output/histogram_", cols[i], "_(", N_obs,  " obs", observacao, ").pdf", sep="")
    
    #  inicia o obj de grafico
    pdf(file=FileName)
    
    nome = paste("Histograma ", cols[i], " (", N_obs," obs", observacao, ")", sep = "")
    hist(x[,i],freq = F, labels = F, main=nome,  xlab = "Media", breaks = 20)
    lines(density(x[,i], adjust = 2), col = "Red", lwd=2)
    dev.off()
  }
  
}

RunSimulation = function (sampleTotal, N_obs, errorType)
{
  # declara uma matrix que vai conter os resultados
  result = matrix(NA, nrow=sampleTotal, ncol=5)
  colnames(result) = c("b0", "b1", "var(b0)", "var(b1)", "SSX")
  
  # looping do processo de regressao para o total de amostras (sampleTotal)
  for (i in 1:sampleTotal)
  {
    # Realiza sorteios da variavel x
    x = rnorm(N_obs, mean = 2, sd = 1)

    # Realiza sorteios dos erros 
    if(errorType==2){
      u = rnorm(N_obs, mean = 0, sd = x^2)
    }
    else if (errorType==3){
      u = -2 + x + rnorm(N_obs, mean = 0, sd = 1)
    }
    else{
      u = rnorm(N_obs, mean = 0, sd = 1)
    }
    #  calcula o y
    y = 10 + 0.1*x + u
    
    # agrupa as informacoes de y e x em um unico data-frame
    data = data.frame(y, x) 
    
    # realiza a regressa de y contra x
    fit = lm("y ~ x", data = data)
    
    # guarda as variaveis de coeficientes e variancia.
    result[i, c("b0", "b1")] = fit$coefficients;
    result[i, c("var(b0)", "var(b1)")] = diag(vcov(fit));
    
    result[i, "SSX"] = sum( (x- mean(x))^2 )
  }
  
  return(result)
}

RunT_Test = function(x, mu, var_x, Titulo)
{
  cat(sprintf(Titulo), "\n")
  # Defino uma estatistica t para o teste
  t = (x - mu) / sqrt(var_x)

  # Verifico qual a porcentagem de individuos que foram rejeitados
  cat(sprintf("\tTotal de rejeicoes: %d\n", sum(t > 1.96 | t < -1.96)))  
  cat(sprintf("\tTotal de amostas: %d\n", length(x)))  
  cat(sprintf("\tPorcentagem de rejeicoes: %f\n", sum(t > 1.96 | t < -1.96)/length(x)))  
}