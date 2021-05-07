# Linpeza de variaveis
rm(list = ls())

source("./InstallPackages.R")


## VAMOS ANALISAR O I DE MORAN 

# carrega shapefile
municipios <- sf::st_read("./ShapeFile/35MUE250GC_SIR.shp")

# Carrega dados de criminalidade
dados.crim.2010 <- read_excel("./dados_crim_sp_2010.xlsx")

# Faz um merge dos dados em uma unica base de dados
crim.sp <- dplyr::full_join(municipios, dados.crim.2010,  by = c("CD_GEOCMU" = "CodIBGE"))



# Retiro ilhabela do conjunto (pois ilhabela não temvizinhos)
# crim_sp.sub <- crim.sp[ ! crim.sp$NOME %in% c("ILHABELA"), ]
crim_sp.sub <- crim.sp

# Define variaveis per capita
crim_sp.sub$crime_pc <- exp(crim_sp.sub$ln_crime)/as.numeric(crim_sp.sub$Populacao)
crim_sp.sub$pib_pc <- exp(crim_sp.sub$ln_pib)/as.numeric(crim_sp.sub$Populacao)


# aloca a media para as observacoes ausentes no banco de dados
crim_sp.sub$gini[! complete.cases(crim_sp.sub$gini)] = mean(crim_sp.sub$gini, na.rm=T)
crim_sp.sub$crime_pc[! complete.cases(crim_sp.sub$crime_pc)] =  mean(crim_sp.sub$crime_pc, na.rm=T)
crim_sp.sub$pib_pc[! complete.cases(crim_sp.sub$pib_pc)] =  mean(crim_sp.sub$pib_pc, na.rm=T)
crim_sp.sub$tx_desemprego[! complete.cases(crim_sp.sub$tx_desemprego)] = mean(crim_sp.sub$tx_desemprego, na.rm=T)
crim_sp.sub$educ[! complete.cases(crim_sp.sub$educ)] = mean(crim_sp.sub$educ, na.rm=T)
crim_sp.sub$gini[! complete.cases(crim_sp.sub$gini)] = mean(crim_sp.sub$gini, na.rm=T)
crim_sp.sub$ln_crime[! complete.cases(crim_sp.sub$ln_crime)] = mean(crim_sp.sub$ln_crime, na.rm=T)

# Retiro ilhabela do conjunto (pois ilhabela não tem vizinhos)
crim_sp.sub <- crim_sp.sub[ ! crim_sp.sub$NOME %in% c("ILHABELA"), ]

# Definicao de vizinhancas
sp_viz <- spdep::poly2nb(as(crim_sp.sub, "Spatial"), row.names = crim_sp.sub$CD_GEOCMU)
xy <- sp::coordinates(as(crim_sp.sub, "Spatial"))
plot(sp_viz, xy)

summary(sp_viz)

# Matrix no esquema binario
wb.binary <- spdep::nb2listw(sp_viz, style = "B", zero.policy = T)
spdep::print.listw(wb.binary, zero.policy = T)

# Matrix no esquema W
wb.w <- spdep::nb2listw(sp_viz, style = "W", zero.policy = T)
spdep::print.listw(wb.binary, zero.policy = T)

# Moran Teste
spdep::moran.test(crim_sp.sub$ln_crime, wb.w, randomisation = F, zero.policy = T, na.action = na.omit)

# monte carlo de Moran
spdep::moran.mc(crim_sp.sub$ln_crime, wb.w, nsim = 99, zero.policy = T, na.action = na.omit)


spdep::geary(crim_sp.sub$ln_crime, wb.w, n = length(wb.w$neighbours), n1=length(wb.w$neighbours), S0 = Szero(wb.w), zero.policy = T)


locMoran <- spdep::localmoran(crim_sp.sub$ln_crime, wb.w)

sids.shade <- GISTools::auto.shading(c(locMoran[,1], -locMoran[,1]), cols = brewer.pal(5, "PRGn"))

choropleth(crim_sp.sub["ln_crime"], locMoran[,1], shading = sids.shade)
choro.legend(-46.5, -20, sids.shade, fmt="%6.2f")
title("Criminalidade (Local Moran's I)", cex.main=2)


Gs = spdep::localG(crim_sp.sub$ln_crime, wb.w)

sids.shade <- GISTools::auto.shading(c(Gs,-Gs), cols = brewer.pal(5, "PRGn"))
choropleth(crim_sp.sub["ln_crime"], Gs, shading = sids.shade)
choro.legend(-46.5, -20, sids.shade, fmt="%6.2f")
title("Criminalidade (G*)", cex.main=2)



