# Carrega livrarias que serao utilizadas
source("./InstallPackages.R")

# Limpeza de variaveis antigas
rm(list=ls())

# funcao para converter de neiborhoord em data frame. 
source("./function_nb_to_df.R")

# tamanho das imagens que vou gerar
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

# imprime em tela o summario dos dados
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
# vamos ficar somente com nossa base em Simple Features
rm(list = c("dados", "municipios"))

# Retiramos ILHABELA
# (Nas palavras de captao fabio: Ilha bela de C é R!!!)
IlhabelaCode = 3520400
Dados.sf <- Dados.sf[!Dados.sf$CD_GEOCMU == IlhabelaCode,]



# nao tenho cemiterios, vou usar os centroides como proxy de cemiterios
Dados.coords <- sf::st_coordinates(sf::st_centroid(Dados.sf))

# Conversao dos dados para o tipo Spatial
Dados.sp <- as(Dados.sf, "Spatial")

# Constroi os neibors para tipo Queen
Dados.nb <- spdep::poly2nb(Dados.sp)

# Data frame com as coordenadas dos cemiterios
coord_df = data.frame(Dados.coords)

# de nome as colunas, eu usei nome de guerra!!!
colnames(coord_df) <- c("Xena", "Yvette")

# Isso só é necessario se quiser imprimir o link entre os centros
df = nb_to_df(Dados.nb, Dados.coords)


# Agora a Magica

# # png([file name]) salva o arquivo em disco
# # deve ser usado conjuntamente com dev.off() ao final
# png("1_ln_Emp.png", width = fig_width, height = fig_height)

# vamos gerar um ggplor como base pricipal nossa simple features
# atenção ao "+" no fim de cada linha 
# teremos 1 unico comando que ocupa varias linhas

# #base de dados principal
# ggplot(Dados.sf) +  
#   # aes = aesthetics é isso que informa o que é que estamos plotando
#   geom_sf(aes(fill = ln_Emp)) + 
#   # aqui estamos dizendo como colorir
#   scale_fill_gradient(low = "blue", high = "white", name = "Emprego (log)") +
#   # aqui plotando mais um layer,
#   # note que este layer tem outra base de dados data=coord_df
#   # e ele usa o x e y definido nas colunas.
#   geom_point(aes(x=Xena, y=Yvette), data=coord_df) +
#   # Se quiser imprimir as conexoes descomenta a proxima linha
#   #geom_segment(aes(x=x, xend=xend, y=y, yend=yend)) +
#   # Nome dos eixos X e Y !! 
#   ylab("Latitude") +
#   xlab("Longitude")


ggplot(Dados.sf) +  
  geom_sf(aes(fill = ln_Emp)) + 
  scale_fill_gradient(low = "blue", high = "white", name = "Emprego (log)") +
  geom_point(aes(x=Xena, y=Yvette), data=coord_df) +
  ylab("Latitude") +
  xlab("Longitude")
# dev.off() 





