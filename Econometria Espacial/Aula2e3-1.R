# Linpeza de variaveis
rm(list = ls())

# Linpeza de variaveis e load das bibliotecas
source("./InstallPackages.R")

# Bibliotecas utilizadas
# library(dplyr)
# # library(magrittr)
# # library(sp)
# # library(rgdal)
# library(spdep)
# # library(GISTools)
# library(readxl)
# library(sf)



## Aula 2

# carrega shapefile
municipios <- sf::st_read("./ShapeFile/35MUE250GC_SIR.shp")

# Carrega dados de criminalidade
dados.crim.2010 <- read_excel("./dados_crim_sp_2010.xlsx")

# Imprime o bounding box do shape de monicipios
sf::st_bbox(municipios)


# imprime a estrutura do shapefile
str(municipios)


# Imprime qual o sistema de coordenadas espaciais utilizado
sf::st_crs(municipios)

# Impressao do mapa utilizando o ggplot2
ggplot(municipios) + geom_sf()

# Ha muito mais que pode ser feito combinando dois mapas.
# Mais detalhes nas notas de aula


# Combinacao de planilhas e shapefiles.

# # Conversao de dados para padrao ITF-8
# dados.crim.2010$municipio <- iconv(dados.crim.2010$municipio, "UTF-8")
# 
# # Deixa nomes em minusculo
# dados.crim.2010$municipio <- tolower(dados.crim.2010$municipio)
# 
# 
# # *** Processo de retirada de ascentos ***
# c_ac <- "áéíóúàèìòùâêîôûãêîôõûç"
# s_ac <- "aeiouaeiouaeiouaeioouc"
# dados.crim.2010 <- dplyr::mutate(as.data.frame(dados.crim.2010), municipio = chartr(c_ac, s_ac, municipio))
# dados.crim.2010 <- dplyr::mutate(as.data.frame(dados.crim.2010), municipio = gsub("-", " ", municipio))
# 
# 
# # Conversao de dados para padrao ITF-8
# municipios$NM_MUNICIP <- iconv(municipios$NM_MUNICIP, "UTF-8")
# 
# # Deixa nomes em minusculo
# municipios$NM_MUNICIP <- tolower(municipios$NM_MUNICIP)
# 
# 
# mun.sp <- dplyr::mutate(as.data.frame(municipios), NM_MUNICIP = chartr(c_ac, s_ac, NM_MUNICIP))
# mun.sp <- dplyr::mutate(as.data.frame(municipios), NM_MUNICIP = gsub("-", " ", NM_MUNICIP))


# Verifica as diferencas entre as base de dados
# Dados que estao na base de dados de criminalidade
# menos os municipios do shapefile
dplyr::setdiff(dados.crim.2010$CodIBGE, municipios$CD_GEOCMU)

# Dados que estao na base do shapelife
# menos os municipios na base de dados de criminalidade
dplyr::setdiff(municipios$CD_GEOCMU, dados.crim.2010$CodIBGE)

# Faz um merge dos dados em uma unica base de dados
crim.sp <- dplyr::full_join(municipios, dados.crim.2010,  by = c("CD_GEOCMU" = "CodIBGE"))

plot(crim.sp["gini"])

plot(municipios)

# constroi os centroides e determinas suas coordenadas
coords <- sf::st_coordinates(sf::st_centroid(municipios))

# Plota as coordenas dos cenntroides
ggplot(as.data.frame(coords), aes(X,Y)) + geom_point()

# Processo desligado por enquanto
if (FALSE){
  
  IDs <- municipios$CD_GEOCMU
  Sy4_nb <- spdep::tri2nb(coords, row.names=IDs)
  
  plot(Sy4_nb, coords)
  
  Sy5_nb <- spdep::graph2nb(soi.graph(Sy4_nb, coords),row.names=IDs)
  plot(Sy5_nb, coords)
  
  Sy5_nb
  
  Sy6_nb <- spdep::graph2nb(gabrielneigh(coords), row.names=IDs)
  plot(Sy6_nb,coords)
  
  Sy7_nb <- spdep::graph2nb(relativeneigh(coords), row.names=IDs)
  plot(Sy7_nb,coords)
  
  # Vizinhos Mais proximos
  # Define a ligacao com 1 vizinho mais proximo.
  Sy8_nb <- spdep::knn2nb(knearneigh(coords, k=1),row.names=IDs)
  plot(Sy8_nb,coords)
  
  Sy9_nb <- spdep::knn2nb(knearneigh(coords, k=2),row.names=IDs)
  plot(Sy9_nb,coords)
  
  Sy10_nb <- spdep::knn2nb(knearneigh(coords, k=3),row.names=IDs)
  plot(Sy10_nb,coords)
  
  
  # Matrix com distancia maxima
  # Constroi um vetor com todas as distancias
  dsts <- unlist(spdep::nbdists(Sy8_nb, coords))
  summary(dsts)
  
  # Distância máxima de um vizinho
  max_1nn <- max(dsts)
  
  # Cálculo e represetnação dos vizinhos a uma ditância máxima
  Sy11_nb <- dnearneigh(coords, d1=0, d2=max_1nn, row.names=IDs)
  plot(Sy11_nb,coords)
  
}

# Retiro ilhabela do conjunto (pois ilhabela não tem vizinhos)
crim_sp.sub <- crim.sp[ ! crim.sp$NOME %in% c("ILHABELA"), ]

# Retira outliers (eu acho que nao deveria fazer isso)
crim_sp.sub$educ[is.na(crim_sp.sub$educ)] <- mean(crim_sp.sub$educ, na.rm=T)

plot(crim_sp.sub$educ, type="l")

# Define crime per capita
crim_sp.sub$crime_pc <- exp(crim_sp.sub$ln_crime)/as.numeric(crim_sp.sub$Populacao)

# Define crime per capita
crim_sp.sub$pib_pc <- exp(crim_sp.sub$ln_pib)/as.numeric(crim_sp.sub$Populacao)

# aloca a media para as observacoes da media
crim_sp.sub$crime_pc[! complete.cases(crim_sp.sub$crime_pc)] =  mean(crim_sp.sub$crime_pc, na.rm=T)
crim_sp.sub$pib_pc[! complete.cases(crim_sp.sub$pib_pc)] =  mean(crim_sp.sub$pib_pc, na.rm=T)
crim_sp.sub$tx_desemprego[! complete.cases(crim_sp.sub$tx_desemprego)] = mean(crim_sp.sub$tx_desemprego, na.rm=T)
crim_sp.sub$educ[! complete.cases(crim_sp.sub$educ)] = mean(crim_sp.sub$educ, na.rm=T)
crim_sp.sub$gini[! complete.cases(crim_sp.sub$gini)] = mean(crim_sp.sub$gini, na.rm=T)

# OLS ignorando a estrutura espacial dos dados, temos:
reg.ols <- lm(crime_pc ~ pib_pc + tx_desemprego + educ + gini, data = crim_sp.sub)
summary(reg.ols)

# Guarda residuos e valores fitados
crim_sp.sub$ols.resid <- reg.ols$residuals
crim_sp.sub$ols.prev <- reg.ols$fitted.values

# Controi matriz de pesos
mun.sp.nb <- spdep::poly2nb(as(crim.sp, "Spatial"))
plot(mun.sp.nb, coords)

# Tentativa de fazer direto com SF, aparentemente funciona. Veja so que precisamos redefinir
# o vetor de centroides pois retiramos ilhabela
# Atualizacao: usar a classe spatial ajuda na nomeacao dos rownames.
sp_viz <- spdep::poly2nb(crim_sp.sub, row.names = crim_sp.sub$CD_GEOCMU)
xy <- sp::coordinates(as(crim_sp.sub, "Spatial"))
plot(sp_viz, xy)

# Matrix no esquema Rook
sp_viz.Rook <- spdep::poly2nb(crim_sp.sub, row.names = crim_sp.sub$CD_GEOCMU, queen = F)
plot(sp_viz.Rook, xy)


# Extração da matrix de pessos neiborhood to List W
# zero.policy:
# if FALSE stop with error for any empty neighbour sets,
# if TRUE permit the weights list to be formed with zero-length weights vectors
lw <- spdep::nb2listw(sp_viz, style ='W', zero.policy = TRUE)


# Conversao de um arquivo Spatial em Simple Features 
crim.sp.spatial = as(crim.sp, "Spatial")
class(crim.sp.spatial)
crim.sp.simpleFeatures <- st_as_sf(crim_sp.sub)
class(crim.sp.simpleFeatures)

# Moran's I para os residuos:
ols.resid.corr <- sp.correlogram(ww$neighbours, crim_sp.sub$ols.resid, order = 4, method = "I",
                                 style = "W", randomisation = TRUE, zero.policy = NULL, spChk=NULL)
plot(ols.resid.corr)

lm.morantest(reg.ols, nb2listw(sp_viz, style="W"), alternative = "two.sided")


# Constroi um raster de dimensao 50 x 50
r.teste <- raster::raster(nrows=50, ncols=50, crs=NA)
# Atribui valores aleatorios (Distrib. Uniforme) para o raster
r.teste <- raster::setValues(r.teste, runif(2500))

plot(r.teste)

# Converte o raster em um Spatial object (? parece que sim.)
r.poly =  rasterToPolygons(r.teste)

# Constroi os neibors do raster para tipo Queen
nb = spdep::poly2nb(r.poly, queen = T)
nb.coords <- sp::coordinates(r.poly)
plot(nb, nb.coords)
nb.lw = spdep::nb2listw(nb, style = "W", zero.policy = T)

# Nao entendi o que seria esse LAYER????
# Faz o teste de I de moram
spdep::moran.test(r.poly$layer, lw)

# Faz o teste de c de geary
spdep::geary.test(r.poly$layer, lw)


