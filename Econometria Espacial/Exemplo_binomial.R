rm(list = ls())
# Ensaio de bernouli
# y: Total de sucessos
# n: Total de ensaios
n <- 500
p <- 0.5

db = tibble(k = 0:n, p = NA, F=NA)

for(i in 1:nrow(db)){
  db[i,2] = dbinom(as.numeric(db[i,1]), n, p)
  if(i == 1)
  { 
    db[i,3] = db[i,2]
  }
  else
  {
    db[i,3] = db[i,2] + db[i-1,3]
  }
  
}


db$norm = dnorm(0:n, mean = n*p, sd = (n*p*(1-p))^0.5, log = FALSE)

ggplot(db, aes(x=k, y=p)) +
   geom_col() +
  geom_line(aes(y=norm)) +
  labs(title="Binomial Distribution", # Title
       subtitle=TeX(sprintf('$n = %d \\; k = %d$', n, y)), # Subtitle
       caption="Source: Economics", # Caption
       y=TeX('$f(\\theta)$'), 
       x=TeX('$\\theta$'),
       color=NULL)


