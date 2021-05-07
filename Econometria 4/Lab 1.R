# Faz a limpeza de dados antigos
rm(list = ls())


# ************************* Packages

library(foreign) # Reading dta 
library(plm)     # Estimations
library(lmtest)  # Cluster-robust Huber/White standard errors, Heteroskedasticity test
library(gplots)  # Plots


# ************************* Import data

nlswork = read.dta('http://www.stata-press.com/data/r11/nlswork.dta')


# ************************* Create a panel data

## id variable (unit): idcode
## time variable: year
pnlswork = plm::pdata.frame(nlswork,c('idcode','year'))

head(pnlswork)

# *********************** New variables

# age^2 = age**2
pnlswork$age2     = pnlswork$age**2
pnlswork$ttl_exp2 = pnlswork$ttl_exp**2
pnlswork$tenure2  = pnlswork$tenure**2
pnlswork$black    = as.numeric(pnlswork$race == 2)

# Verify if there is missing data
cat(sprintf('Existem %d elementos n.a. na base de dados.',sum(is.na(pnlswork$black))))

# *********************** Explore data
summary(pnlswork)

# Some observations

head(pnlswork)
tail(pnlswork)
# Index

head(attr(pnlswork, "index"))
tail(attr(pnlswork, "index"))

#*********************** Model estimation

# pooled OLS ("pooling")
# fixed effects model ("within")
# random effects model ("random")
# first-difference model ("fd")


# *********************** Pooled OLS with corrected SE

pooled_ols = plm(ln_wage ~ grade + age + age2 + ttl_exp +
                 ttl_exp2 + tenure + tenure2 + black + 
                 not_smsa + south, model="pooling", data=pnlswork)

summary(pooled_ols)


# Recall Pooled OLS does not consider heterogeneity across groups or time

# Example: heterogeneity across year

plotmeans(ln_wage ~ year, main="Heterogeineity across years", data=pnlswork)


## Heteroskedasticity test

# Breusch-Pagan test
# H0: homoskedastic

bptest(pooled_ols)


#************************* Fixed Effect

fe = plm(ln_wage ~ grade + age + age2 + ttl_exp +
                   ttl_exp2 + tenure + tenure2 + black + 
                   not_smsa + south, model="within", data=pnlswork)

# Note that time-constant variables (grade)  are not identifiable by FE

summary(fe)



#*** Cluster-robust Huber/White SE

## cluster="group" defines the clusters by the individual identifier

## we restimate the model considering the following vcov

coeftest(fe,vcov=vcovHC(fe,cluster = "group"))


#************************* Random effects

re = plm(ln_wage ~ grade + age + age2 + ttl_exp +
           ttl_exp2 + tenure + tenure2 + black + 
           not_smsa + south,model="random",data=pnlswork)

summary(re)

#*** Cluster-robust Huber/White SE

coeftest(re,vcov=vcovHC(re,cluster = "group"))


#************************* Especification Test

#*** Breusch and Pagan test (OLS vs RE)

# H0: No significant difference across units 
## (variances across entities is zero - no panel effect)

plmtest(pooled_ols, type=c("bp"))

# Reject H0: RE is more appropriate


#*** Hausman test (RE vs FE)

# Estimate RE and FE, then compare the estimatives

# H0: the preferred model is RE
# H1: preferred model is FE

# Tests whether the unique errors are correlated with the regressors
# Ho: they are not

phtest(fe,re)

# We use FE


#*** Serial correlation

# H0: no serial correlation

pbgtest(fe)

# Reject H0 - serial correlation


## Wooldridge test for autocorrelation in panel data

pwartest(ln_wage ~ grade + age + age2 + ttl_exp +
           ttl_exp2 + tenure + tenure2 + black + 
           not_smsa + south, data = pnlswork)



#************************* First difference

fd = plm(ln_wage ~ grade + age + age2 + ttl_exp +
           ttl_exp2 + tenure + tenure2 + black + 
           not_smsa + south,model="fd",data=pnlswork)

summary(fd)



