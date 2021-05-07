# ***************************************************
# Lista 3 - QUESTAO 6
# Nome: Bruno Tebaldi de Queiroz Barbosa
# COD: 174887
# 
# Disciplina: Econometria 1
# TA: Luiz Fantozzi Alvarez
# ***************************************************

# Limpa as variaveis anteriores
rm(list = ls())

# Carrega bibliotecas externas
library(dplyr)

# Importa base de dados
CSHomePrice = read.csv("./Lista 3/temp_CSHomePrice.mat.csv", stringsAsFactors=FALSE)
Probs = read.delim("./Lista 3/probs.txt",  stringsAsFactors=FALSE)
# colnames(CSHomePrice)

# Aggregate the data by year and circuit
CS_aggregated = dplyr::group_by(CSHomePrice, year, circuit) %>% dplyr::summarise_all(funs(mean))
# colnames(CS_aggregated)

# Run a regression on logpriceindex ~ numpro_casecat_12
mdl_1 = lm(logpriceindex ~ numpro_casecat_12, data = CS_aggregated)
print(summary(mdl_1))

# O coeficiente encontrado nao tem relacao causal, pois 
# eh esperado que o preco dos imoveis suba por causa da
# decisao judicial, porem pode haver incentivo dos Juizes
# em fazer uma decisao a favor do plaintiff ou contra 
# caso o juiz pense em realizar negocios no mercado imobiliario.

# Agrega as duas bases
# FullData = dplyr::full_join(CS_aggregated, Probs, by= c("year" = "Syear", "circuit" = "Scircuit"))
FullData = dplyr::inner_join(CS_aggregated, Probs, by= c("year" = "Syear", "circuit" = "Scircuit"))

# colnames(FullData)

# apaga as bases pasadas para nao ocupar espaco
rm(list = c("CS_aggregated", "CSHomePrice", "Probs"))

varControls_01 = c("missing_cy_12", "numcasecat_12")
varControls_99 = c("prob_1x_dem", "prob_2x_dem", "prob_3x_dem",
                   "prob_1x_female", "prob_2x_female", "prob_3x_female",
                   "prob_1x_nonwhite", "prob_2x_nonwhite", "prob_3x_nonwhite",
                   "prob_1x_black", "prob_2x_black", "prob_3x_black",
                   "prob_1x_jewish", "prob_2x_jewish", "prob_3x_jewish",
                   "prob_1x_catholic", "prob_2x_catholic", "prob_3x_catholic",
                   "prob_1x_noreligion", "prob_2x_noreligion", "prob_3x_noreligion",
                   "prob_1x_instate_ba", "prob_2x_instate_ba", "prob_3x_instate_ba",
                   "prob_1x_ba_public", "prob_2x_ba_public", "prob_3x_ba_public",
                   "prob_1x_jd_public", "prob_2x_jd_public", "prob_3x_jd_public",
                   "prob_1x_elev", "prob_2x_elev", "prob_3x_elev",
                   "prob_1x_female_black", "prob_2x_female_black", "prob_3x_female_black",
                   "prob_1x_female_noreligion", "prob_2x_female_noreligion", "prob_3x_female_noreligion", 
                   "prob_1x_black_noreligion", "prob_2x_black_noreligion", "prob_3x_black_noreligion", 
                   "prob_1x_female_jd_public", "prob_2x_female_jd_public", "prob_3x_female_jd_public",
                   "prob_1x_black_jd_public", "prob_2x_black_jd_public", "prob_3x_black_jd_public",
                   "prob_1x_noreligion_jd_public", "prob_2x_noreligion_jd_public", "prob_3x_noreligion_jd_public",
                   "prob_1x_mainline", "prob_2x_mainline", "prob_3x_mainline",
                   "prob_1x_evangelical", "prob_2x_evangelical", "prob_3x_evangelical",
                   "prob_1x_protestant", "prob_2x_protestant", "prob_3x_protestant");


f1 = formula(paste(c("numpro_casecat_12 ~ numpanels1x_dem", varControls_01, varControls_01, varControls_99), collapse = " + "))
mdl_1stg = lm(formula = f1, data = FullData)
print(summary(mdl_1stg))

FullData[, "x_hat"] = mdl_1stg$fitted.values
f2 = formula(paste(c("logpriceindex ~ x_hat", varControls_01, varControls_01, varControls_99), collapse = " + "))
mdl_2stg = lm(formula = f2, data = FullData)
print(summary(mdl_2stg))

# podemos notar no primeiro estagio que a variavel em questao nao satisfaz 
# os requisitos de um instrumento, sendo considerado um instrumento fraco.

# vemos que a covariancia de numpanels1x_dem com numpro_casecat_12 é insignificante,
# sendo assim, o instrumento em questao (numpanels1x_dem) nao satisfaz a condicao
# de ser significantemente correlacionado com a variavel.


