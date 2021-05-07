###############################################################################
# Class: Econometrics II
# Professor: Cristine Campos de Xavier Pinto
# TA: Luis Alvarez
# Code by: Bruno Tebaldi de Queiroz Barbosa
#
# Topic: Lista 1 - Questao 12
###############################################################################

# Clear all past variables
rm(list = ls())

# Load the data base
library(readr)
library(dplyr)
library(sandwich)
smokeData <- read_csv("Database/smoke.csv")
head(smokeData)

# I advise you not to use packages except for those in R’s base installation. If you decide to do it though, you
# must explain carefully what your package is calculating at each step. 
# Take your time interpreting what’s being asked and write down your model, scores and Hessians.
# The idea here is to model how many cigarettes the individual smokes per day as a function of
# price and other covariates.
# In this case, we’re counting the number of cigarettes, so a Poisson-like model may seem
# suitable. 

# Questao a)

mdl_ols = lm(cigs ~ lcigpric + lincome + white + educ + age + agesq + restaurn,  data=smokeData)
robust_errors_mdl_ols = diag(sandwich::vcovHC(mdl_ols, "HC1"))^0.5

# Questao b)

X = as.matrix(select(smokeData, lcigpric, lincome, white, educ, age, agesq, restaurn))
Y = as.matrix(select(smokeData, cigs))

beta_0 = rep(0.0,dim(X)[2])

#Computes individual score
score.i = function(b,y,X)
{
  return(as.vector(y - exp(X%*%b))*X)
}

#Computes sample score
score.sample =function(b,y,X)
{
  return(colSums(score.i(b,y,X)))
}

lkl.i = function(b, y, X)
{
  return(dpois(y, exp(X%*%b), log = T))
}



#Sample likelihood
lkl.sample = function(b, y, X)
{
  return(sum(lkl.i(b,y,X)))
}


#Optimisation w/o passing gradient. We need to set fnscale = -1 in control so we maximise
#instead of minimise (or just put a minus sign in the previous functions)
opt.1 = optim(par = beta_0, fn =  lkl.sample, y = Y, X = X, method = "BFGS", control = list("fnscale"=-1), hessian = T)

opt.1$hessian

b.mle = opt.1$par
se.mle = sqrt(diag(-solve(opt.1$hessian)))
table.results = cbind("Beta" = b.mle, "SE" = se.mle, "Z-stat" = b.mle/se.mle)

print(table.results)


# Questao c e d)


# Desde que E [y | x] = exp (x′ beta) e que a média condicional seja identificada
# beta MLE estima consistentemente a média condicional, mesmo se a distribuição verdadeira não for Poisson


  #Runs MLE
  mle.mc = optim(fn =  lkl.sample, par = beta_0, method = "BFGS", hessian=T, control = list("fnscale"=-1), y = Y, X = X)
  
  #Correct SEs
  score.mat = score.i(mle.mc$par, Y, X)

  #Sandwich estimator for Avar
  avar.correct = inv.Hessian%*%(t(score.mat)%*%score.mat)%*%inv.Hessian
  se.right = sqrt(diag(avar.correct))
  
  
# QUestao e
hist(Y)
NonSmokers = filter(smokeData, cigs==0)
dim(NonSmokers)

# um modelo TOBIT pode ser elaborado para acomodar os dados de nao fumantes.