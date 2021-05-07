###############################################################################
# Class: Econometrics II
# Professor: Cristine Campos de Xavier Pinto
# TA: Luis Alvarez
# Code by: Bruno Tebaldi de Queiroz Barbosa
#
# Topic: Lista 2 - Questao 10
###############################################################################

# Clear all past variables
rm(list = ls())

#Function that returns the moment function evaluated at an observation
dgp <-function(n)
{

z1 = rnorm(n, 0, 1)
z2 = rnorm(n, 0, 1)
z3 = rnorm(n, 0, 1)
v = rnorm(n, 0, 1)
eps = rnorm(n, 0, 1)

x = v + z1 + z2 + z3

y = x + z2 + eps

return(cbind(y, x, z1, z2, z3))
}


#Function that returns the moment function evaluated at an observation
moment.observation <-function(b, y, X)
{
  return(as.vector(y - X%*%b)*X)
}

#GMM objective function, where W is a 3x3 weighing matrix passed by the user
gmm.objective <- function(b, y, X, W)
{
  gn = colMeans(moment.observation(b,y,X))
  return(t(gn)%*%W%*%gn)
}

#Jacobian of moment function
jacob.moment.observation <- function(b,y,X)
{
  return(t(cbind(X, X[,2]^2)*(-exp(X%*%b))) )
}

# Questao a)

#Let's run a NLS of y on (1,x1) to get a starting point for the optimisation method.
#In practice, a good choice of starting point is really important for GMM to converge, as a highly nonlinear function may have multiple local minima
#nls.cmean = nls(y~exp(beta0+beta1*x1), start =  list("beta0"=0, "beta1"=1),data = as.data.frame(data.gmm))

myData = dgp(300)
X_all = as.matrix(myData[, c("x")])
Y = myData[, c("y")]
beta_0 = rep(0, ncol(X_all))


#First-step GMM
gmm.a = optim(fn = gmm.objective, par = beta_0, method = "BFGS",  y = Y,
               X = X_all, W = diag(1))

gmm.a


# Questao b)

moment.observation2 <-function(b, y, X, Z)
{
  return(as.vector(y - X%*%b)*Z)
}

gmm.objective2 <- function(b, y, X, Z, W)
{
  gn = colMeans(moment.observation2(b,y,X,Z))
  return(t(gn)%*%W%*%gn)
}

rejNumber = 0;
rejNumber.z1z2 = 0;
rejNumber.z1z3 = 0;
rejNumber.z2z3 = 0;
rept = 10
for(i in 1:rept){
  
myData = dgp(300)
X_all = as.matrix(myData[, c("x")])
Z = myData[, c("x", "z1", "z2", "z3")]
Y = myData[, c("y")]
beta_0 = rep(0, ncol(X_all))


#First-step GMM
gmm.fs = optim(fn = gmm.objective, par = beta_0, method = "BFGS",  y = Y, X = X_all, W = diag(1))

optimal.weight.hat = solve(t(moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z))%*%moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z)/300)


#Second-step GMM using estimated optimal weight
gmm.second.stage = optim(fn = gmm.objective2, par = gmm.fs$par, method = "BFGS",  y = Y,
                         X = X_all, Z=Z, W = optimal.weight.hat)

optimal.weight.hat.z1z2 = solve(t(moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z[,c("x","z1", "z2")]))%*%moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z[,c("x","z1", "z2")])/300)
gmm.second.stage.z1z2 = optim(fn = gmm.objective2, par = gmm.fs$par, method = "BFGS",  y = Y,
                         X = X_all, Z=Z[,c("x","z1", "z2")], W = optimal.weight.hat.z1z2)

optimal.weight.hat.z1z3 = solve(t(moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z[,c("x","z1", "z3")]))%*%moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z[,c("x","z1", "z3")])/300)
gmm.second.stage.z1z3 = optim(fn = gmm.objective2, par = gmm.fs$par, method = "BFGS",  y = Y,
                              X = X_all, Z=Z[,c("x","z1", "z3")], W = optimal.weight.hat.z1z3)

optimal.weight.hat.z2z3 = solve(t(moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z[,c("x","z2", "z3")]))%*%moment.observation2(gmm.fs$par, y = Y, X = X_all, Z=Z[,c("x","z2", "z3")])/300)
gmm.second.stage.z2z3 = optim(fn = gmm.objective2, par = gmm.fs$par, method = "BFGS",  y = Y,
                              X = X_all, Z=Z[,c("x","z2", "z3")], W = optimal.weight.hat.z2z3)

gmm.second.stage

C = matrix(0, 4, 1)
C[1,1] =  -mean(X_all*X_all)
C[2,1] =  -mean(Z[,"z1"]*X_all)
C[3,1] =  -mean(Z[,"z2"]*X_all)
C[4,1] =  -mean(Z[,"z3"]*X_all)

C.z1z2 = matrix(0, 3, 1)
C.z1z2[1,1] =  -mean(X_all*X_all)
C.z1z2[2,1] =  -mean(Z[,"z1"]*X_all)
C.z1z2[3,1] =  -mean(Z[,"z2"]*X_all)

C.z1z3 = matrix(0, 3, 1)
C.z1z3[1,1] =  -mean(X_all*X_all)
C.z1z3[2,1] =  -mean(Z[,"z1"]*X_all)
C.z1z3[3,1] =  -mean(Z[,"z3"]*X_all)

C.z2z3 = matrix(0, 3, 1)
C.z2z3[1,1] =  -mean(X_all*X_all)
C.z2z3[2,1] =  -mean(Z[,"z2"]*X_all)
C.z2z3[3,1] =  -mean(Z[,"z3"]*X_all)



Wald_stat = (gmm.second.stage$par - 1) * solve(t(C)%*%solve(optimal.weight.hat)%*%C) * (gmm.second.stage$par - 1) * (1/300)

Wald_stat.z1z2 = (gmm.second.stage.z1z2$par - 1) * solve(t(C.z1z2)%*%solve(optimal.weight.hat.z1z2)%*%C.z1z2) * (gmm.second.stage.z1z2$par - 1) * (1/300)

Wald_stat.z1z3 = (gmm.second.stage.z1z3$par - 1) * solve(t(C.z1z3)%*%solve(optimal.weight.hat.z1z3)%*%C.z1z3) * (gmm.second.stage.z1z3$par - 1) * (1/300)

Wald_stat.z2z3 = (gmm.second.stage.z2z3$par - 1) * solve(t(C.z2z3)%*%solve(optimal.weight.hat.z2z3)%*%C.z2z3) * (gmm.second.stage.z2z3$par - 1) * (1/300)

if(pchisq(Wald_stat,1)>=0.95){rejNumber = rejNumber + 1}

if(pchisq(Wald_stat.z1z2,1)>=0.95){rejNumber.z1z2 = rejNumber.z1z2 + 1}
if(pchisq(Wald_stat.z1z3,1)>=0.95){rejNumber.z1z3 = rejNumber.z1z3 + 1}
if(pchisq(Wald_stat.z2z3,1)>=0.95){rejNumber.z2z3 = rejNumber.z2z3 + 1}

}

rejRate = rejNumber/ rept
rejRate.z1z2 = rejNumber.z1z2/ rept
rejRate.z1z3 = rejNumber.z1z3/ rept
rejRate.z2z3 = rejNumber.z2z3/ rept


