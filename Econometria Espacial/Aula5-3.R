# Linpeza de variaveis
rm(list = ls())

source("./InstallPackages.R")

# Exemplo beta-binomial 

# Parametros da distribuicao beta
# Define alpha e beta
alpha <- 1.6
beta <- 4.1

# Ensaio de bernouli
# y: Total de sucessos
# n: Total de ensaios
y <- 5
n <- 100

# Compute a sequence of about n+1 equally spaced ‘round’ values which cover the range 
# of the values in x. The values are chosen so that they are 1, 2 or 5 times a power of 10.
theta_p <- pretty(c(0,1), 100)


# layout(rbind(c(1,2), c(3,3)), respect = TRUE)

# prior:
# med_prior <- alpha/(alpha + beta)
# var_prior <- alpha*beta/((alpha+beta)^2*(alpha+beta+1))
# plot(theta_p, dbeta(theta_p, alpha, beta), type='l',
#      main=paste("prior: média = ", round(med_prior, digits=2), ", var = ", round(var_prior, digits=3)),
#      xlab=expression(theta), ylab=expression(p(theta)))


# Density curve
ggplot(data.frame(t=theta_p, d=dbeta(theta_p, alpha, beta), p=pbeta(theta_p, alpha, beta)), aes(x=t)) +
  geom_line(aes(y=d)) +
  labs(title="Beta Distribution", # Title
       subtitle=TeX(sprintf('$\\alpha = %0.2f \\; \\beta = %0.2f$', alpha, beta)), # Subtitle
       caption="Source: Economics", # Caption
       y=TeX('$f(\\theta)$'), 
       x=TeX('$\\theta$'),
       color=NULL)


# verossimilhanca:
y_p <- seq(0, n)
# plot(theta_p, dbinom(y, n, theta_p), type='l',
#      main=paste("verossimilhança: ", "y =", y, ", N = ", n),
#      xlab=expression(theta), ylab=expression(paste("p(y","|",theta,")")))


ggplot(data.frame(t=theta_p, d=dbinom(y, n, theta_p)), aes(x=t)) +
  geom_line(aes(y=d)) +
  labs(title="Binomial Distribution", # Title
       subtitle=TeX(sprintf('$n = %d \\; k = %d$', n, y)), # Subtitle
       caption="Source: Economics", # Caption
       y=TeX('$f(\\theta)$'), 
       x=TeX('$\\theta$'),
       color=NULL)



# posterior:
# alpha_post <- y + alpha - 1
# beta_post <- n - y + beta - 1
# med_post <- alpha_post/(alpha_post + beta_post)
# var_post <- alpha_post*beta_post/((alpha_post+beta_post)^2*(alpha_post+beta_post+1))
# plot(theta_p, theta_p^(y+alpha-1)*(1 - theta_p)^(n-y+beta-1), type='l',
#      main=paste("posterior: média = ", round(med_post, digits=2), ", var = ", round(var_post, digits=3)),
#      xlab=expression(theta), ylab=expression(paste("p(", theta,"|",y,")")))

ggplot(data.frame(t=theta_p, d=theta_p^(y+alpha-1)*(1 - theta_p)^(n-y+beta-1)), aes(x=t)) +
  geom_line(aes(y=d)) +
  labs(title="Posterior Bistribution", # Title
       subtitle="From Binomial & Beta", # Subtitle
       caption="Source: Economics", # Caption
       y=TeX('$f(\\theta)$'), 
       x=TeX('$\\theta$'),
       color=NULL)




ggplot(data.frame(t=theta_p, d=dbinom(y, n, theta_p)), aes(x=t)) +
  geom_histogram() +
  labs(title="Binomial Distribution", # Title
       subtitle=TeX(sprintf('$n = %d \\; k = %d$', n, y)), # Subtitle
       caption="Source: Economics", # Caption
       y=TeX('$f(\\theta)$'), 
       x=TeX('$\\theta$'),
       color=NULL)




