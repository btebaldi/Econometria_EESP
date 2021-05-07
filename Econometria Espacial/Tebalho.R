# Carrega livrarias que serao utilizadas
source("./InstallPackages.R")

# Limpeza de variaveis antigas
rm(list=ls())

source("./function_nb_to_df.R")

fig_width = 800
fig_height = 480


# carrega o shapefile 
municipios <- sf::st_read("./ShapeFile/35MUE250GC_SIR.shp")

# carrega o a base de dados
dados <- read_excel("./Dados.xlsx", range = "B2:AD647", sheet = "Dados")

# Deixa algumas variaveis em log
dados$ln_power = log(dados$Power)
dados$ln_Emp = log(dados$Emp)
dados$ln_PIB_pc = log(dados$PIB_pc)
dados$ln_Saude_pc = log(dados$Saude_pc)

summary(dados)

# Verifica as diferencas entre as base de dados
# Dados que estao na base de dados de criminalidade
# menos os municipios do shapefile
dplyr::setdiff(dados$CodIbge, municipios$CD_GEOCMU)

# Dados que estao na base do shapelife
# menos os municipios na base de dados de criminalidade
dplyr::setdiff(municipios$CD_GEOCMU, dados$CodIbge)

# Faz um merge dos dados em uma unica base de dados
Dados.sf <- dplyr::full_join(municipios, dados,  by = c("CD_GEOCMU" = "CodIbge"))

# Retira da memoria variaveis que nao serao mais utilizadas
rm(list = c("dados", "municipios"))

# Retiramos ILHABELA
IlhabelaCode = 3520400
Dados.sf <- Dados.sf[!Dados.sf$CD_GEOCMU == IlhabelaCode,]


# Fazemos a impressao do grafico de emprego
png("1_ln_Emp.png", width = fig_width, height = fig_height)
ggplot(Dados.sf) + geom_sf(aes(fill = ln_Emp)) +
  scale_fill_gradient(low = "red", high = "green", name = "Emprego (log)")
dev.off()

#  construcao dos poligonos e coordenadas
Dados.coords <- sf::st_coordinates(sf::st_centroid(Dados.sf))

# Conversao dos dados para o tipo Spatial
Dados.sp <- as(Dados.sf, "Spatial")

# Constroi os neibors para tipo Queen
Dados.nb <- spdep::poly2nb(Dados.sp)

coord_df = data.frame(Dados.coords)
colnames(coord_df) <- c("Xena", "Yvette")
df = nb_to_df(Dados.nb, Dados.coords)

png("2_queen.png", width = fig_width, height = fig_height)
# plot(Dados.nb, Dados.coords)
ggplot(df) + geom_segment(aes(x=x, xend=xend, y=y, yend=yend)) +
  geom_point(aes(x=Xena, y=Yvette), data=coord_df) +
  ylab("Latitude") +
  xlab("Longitude")
dev.off()
rm(list = c("coord_df", "df"))

# Controi os neibors para 3 knn
IDs <- Dados.sf$CD_GEOCMU
Dados.nb.3knn <- spdep::knn2nb(knearneigh(Dados.coords, k=3),row.names=IDs)

# plot(Dados.nb.3knn,Dados.coords)

coord_df = data.frame(Dados.coords)
colnames(coord_df) <- c("Xena", "Yvette")
df = nb_to_df(Dados.nb.3knn, Dados.coords)

png("3_3knn.png", width = fig_width, height = fig_height)
# plot(Dados.nb, Dados.coords)
ggplot(df) + geom_segment(aes(x=x, xend=xend, y=y, yend=yend)) +
  geom_point(aes(x=Xena, y=Yvette), data=coord_df) +
  ylab("Latitude") +
  xlab("Longitude")
dev.off()
rm(list = c("coord_df", "df"))

# Construção da matrix de pesos
lw.queen = nb2listw(Dados.nb, zero.policy = F)
mw.df <- read_excel("./Matrix_conex.xlsx", range = "C3:XV646", sheet = "Fuck_Ilhabela", col_names = F)
mw.names <- read_excel("./Matrix_conex.xlsx", range = "B3:B646", sheet = "Fuck_Ilhabela", col_names = F) 
mw.connex = as.matrix(mw.df)
dim(mw.connex)
rownames(mw.connex) = mw.names[[1]]
colnames(mw.connex) = rownames(mw.connex)
lw.connex = spdep::mat2listw(mw.connex)
lw.3knn = nb2listw(Dados.nb.3knn, zero.policy = F)
rm(list=c("mw.df","mw.names", "mw.connex", "IDs"))

# Nao entendi o que seria esse LAYER????
# Faz o teste de I de moram
spdep::moran.test(Dados.sf$ln_Emp, lw.queen)
spdep::moran.test(Dados.sf$ln_Emp, lw.connex)
spdep::moran.test(Dados.sf$ln_Emp, lw.3knn)

# Faz o I de moram por monte carlo
moran.mc(Dados.sf$ln_Emp, lw.queen, nsim = 1000, zero.policy = TRUE, na.action = na.omit)
moran.mc(Dados.sf$ln_Emp, lw.connex, nsim = 1000, zero.policy = TRUE, na.action = na.omit)
moran.mc(Dados.sf$ln_Emp, lw.3knn, nsim = 1000, zero.policy = TRUE, na.action = na.omit)

# Faz o teste de c de geary
spdep::geary.test(Dados.sf$ln_Emp, lw.queen)
spdep::geary.test(Dados.sf$ln_Emp, lw.connex)
spdep::geary.test(Dados.sf$ln_Emp, lw.3knn)

# Calculo o moran local para matrix Queen
locMoran <- spdep::localmoran(Dados.sf$ln_Emp, lw.queen)

# Determio os valores minimo e maximo do moran local
min_moran = min(locMoran[,1])
max_moran = max(locMoran[,1])

# Determino a quantidade de categorias
nBreaks = 5

# Uso a biblioteca GiStools para pegar os pontos de corte
sids.shade <- GISTools::auto.shading(c(locMoran[,1],-locMoran[,1]), cols=brewer.pal(nBreaks,"PRGn"))

# Adiciono as informacoes ao dataframe
Dados.sf$localMoranQueen = locMoran[,1]
Dados.sf$localMoranQueenFactor = cut(locMoran[,1], breaks=c(min_moran, sids.shade$breaks, max_moran), include.lowest=T)

# Gera grafico do moran local para queen
png("4_localMoran_queen.png" , width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = localMoranQueenFactor)) +
  scale_fill_manual(values=sids.shade$cols, name = "Local moran")
dev.off()

# remove variaveis utilizadas para evitar poluicao futura
rm(list = c("sids.shade", "min_moran", "max_moran", "locMoran"))


# Calculo o moran local para matrix de connexao
locMoran <- spdep::localmoran(Dados.sf$ln_Emp, lw.connex)

# Determio os valores minimo e maximo do moran local
min_moran = min(locMoran[,1])
max_moran = max(locMoran[,1])

# Uso a biblioteca GiStools para pegar os pontos de corte
sids.shade <- GISTools::auto.shading(c(locMoran[,1],-locMoran[,1]), cols=brewer.pal(nBreaks,"PRGn"))

# Adiciono as informacoes ao dataframe
Dados.sf$localMoranConnex = locMoran[,1]
Dados.sf$localMoranConnexFactor = cut(locMoran[,1], breaks=c(min_moran, sids.shade$breaks, max_moran), include.lowest=T)

# Gera grafico do moran local para queen
png("5_localMoran_connex.png" , width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = localMoranConnexFactor)) +
  scale_fill_manual(values=sids.shade$cols, name = "Local moran")
dev.off()

# remove variaveis utilizadas para evitar poluicao futura
rm(list = c("sids.shade", "min_moran", "max_moran", "locMoran"))

# Calculo o moran local para matrix 3KNN
locMoran <- spdep::localmoran(Dados.sf$ln_Emp, lw.3knn)

# Determio os valores minimo e maximo do moran local
min_moran = min(locMoran[,1])
max_moran = max(locMoran[,1])

# Uso a biblioteca GiStools para pegar os pontos de corte
sids.shade <- GISTools::auto.shading(c(locMoran[,1],-locMoran[,1]), cols=brewer.pal(nBreaks,"PRGn"))

# Adiciono as informacoes ao dataframe
Dados.sf$localMoran3Knn = locMoran[,1]
Dados.sf$localMoran3KnnFactor = cut(locMoran[,1], breaks=c(min_moran, sids.shade$breaks, max_moran), include.lowest=T)


# Gera grafico do moran local para queen
png("6_localMoran_3knn.png" , width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = localMoran3KnnFactor)) +
  scale_fill_manual(values=sids.shade$cols, name = "Local moran")
dev.off()

# remove variaveis utilizadas para evitar poluicao futura
rm(list = c("sids.shade", "min_moran", "max_moran", "locMoran"))

# Regressoes

# Regressao 1) OLS simples
Ols.Simplaum <- lm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf)
summary(Ols.Simplaum)


# Regressao 2.1) SAR (Queen Matrix)
reg.SAR.queen <- spautolm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf, 
                      listw = lw.queen, family = "SAR", zero.policy = TRUE)
summary(reg.SAR.queen)
moran.test(reg.SAR.queen$fit$residuals, lw.queen)

# Regressao 2.2) SAR (Connex Matrix)
reg.SAR.connex <- spautolm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf, 
                      listw = lw.connex, family = "SAR", zero.policy = TRUE)
summary(reg.SAR.connex)
moran.test(reg.SAR.connex$fit$residuals, lw.connex)

# Regressao 2.3) SAR (3KNN Matrix)
reg.SAR.3knn <- spautolm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf, 
                      listw = lw.3knn, family = "SAR", zero.policy = TRUE)
summary(reg.SAR.3knn)
moran.test(reg.SAR.3knn$fit$residuals, lw.3knn)

# Regressao 3.1) CAR (Queen Matrix)
reg.CAR.queen <- spautolm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf,
                          family = "CAR", listw = lw.queen, zero.policy = TRUE)
summary(reg.CAR.queen)

# Regressao 3.2) CAR (Connex Matrix)
reg.CAR.connex <- spautolm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf,
                           family = "CAR", listw = lw.connex, zero.policy = TRUE)
summary(reg.CAR.connex)

# Regressao 3.3) CAR (3NN Matrix)
reg.CAR.3knn <- spautolm(ln_Emp~1+Anaf+Educ+ln_power+Idh+Furto+PIB_pc+Saude_pc+Longevidade+Gini, data = Dados.sf,
                         family = "CAR", listw = lw.3knn, zero.policy = TRUE)
summary(reg.CAR.3knn)


nBreaks = 7

# Grafico de previsto vs Observados
colorPalette=brewer.pal(nBreaks,"PRGn")
cuts = quantile(Dados.sf$ln_Emp, probs=seq(0, 1, 1/nBreaks))
cuts[1] = -Inf
cuts[nBreaks+1] = Inf

# Adiciono as informacoes de previsao ao dataframe
Dados.sf$RegSarQueenPrev = reg.SAR.queen$fit$fitted.values
Dados.sf$RegSarConnexPrev = reg.SAR.connex$fit$fitted.values
Dados.sf$RegSar3KNNPrev = reg.SAR.3knn$fit$fitted.values

# # Determio os valores minimo e maximo do moran local
# min_reg = min(Dados.sf$RegSarQueenPrev, Dados.sf$RegSarConnexPrev, Dados.sf$RegSar3KNNPrev, Dados.sf$ln_Emp, Dados.sf$RegCarQueenPrev, Dados.sf$RegCarConnexPrev, Dados.sf$RegCar3KNNPrev)
# max_reg = max(Dados.sf$RegSarQueenPrev, Dados.sf$RegSarConnexPrev, Dados.sf$RegSar3KNNPrev, Dados.sf$ln_Emp, Dados.sf$RegCarQueenPrev, Dados.sf$RegCarConnexPrev, Dados.sf$RegCar3KNNPrev)

# Determino os factores do SAR
Dados.sf$ln_EmpFactor = cut(Dados.sf$ln_Emp, breaks=cuts, include.lowest=T)
Dados.sf$SarQueenPrevFactor = cut(Dados.sf$RegSarQueenPrev, breaks=cuts, include.lowest=T)
Dados.sf$SarConnexPrevFactor = cut(Dados.sf$RegSarConnexPrev, breaks=cuts, include.lowest=T)
Dados.sf$Sar3KnnPrevFactor = cut(Dados.sf$RegSar3KNNPrev, breaks=cuts, include.lowest=T)

# Adiciono as informacoes de previsao ao dataframe
Dados.sf$RegCarQueenPrev = reg.CAR.queen$fit$fitted.values
Dados.sf$RegCarConnexPrev = reg.CAR.connex$fit$fitted.values
Dados.sf$RegCar3KNNPrev = reg.CAR.3knn$fit$fitted.values

# Determino os factores do CAR
Dados.sf$CarQueenPrevFactor = cut(Dados.sf$RegCarQueenPrev, breaks=cuts, include.lowest=T)
Dados.sf$CarConnexPrevFactor = cut(Dados.sf$RegCarConnexPrev, breaks=cuts, include.lowest=T)
Dados.sf$Car3KnnPrevFactor = cut(Dados.sf$RegCar3KNNPrev, breaks=cuts, include.lowest=T)


# Impressao dos graficos
png("7-1_Observed_data.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = ln_EmpFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()

png("7-2-1_Fited_queen.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = SarQueenPrevFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()

png("7-2-2_Fited_connex.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = SarConnexPrevFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()

png("7-2-3_Fited_3knn.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = Sar3KnnPrevFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()


png("7-3-1_Fited_queen.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = CarQueenPrevFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()

png("7-3-2_Fited_connex.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = CarConnexPrevFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()

png("7-3-3_Fited_3knn.png", width = fig_width, height = fig_height)
ggplot(data = Dados.sf) +
  geom_sf(aes(fill = Car3KnnPrevFactor)) +
  scale_fill_manual(values=colorPalette, name = "Values")
dev.off()

# remove variaveis utilizadas para evitar poluicao futura
# rm(list = c("colorPalette", "cuts", "nBreaks"))
