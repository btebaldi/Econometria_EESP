# sp : Spatial
# sf : Simple Features
# ggmap : Google maps
# tidyverse : Tideverse
# magrittr : Pipe enabler
# gstat :
# latex2exp : 
# spdep : 
# readxl : Read excel files
# raster
# GISTools
# RColorBrewer: Paleta de cores

myLybraries = c("sp", "ggmap", "tidyverse", "magrittr", "gstat", "latex2exp", "spdep", "sf", "readxl", "raster", "GISTools", "knitr", "spgwr", "RColorBrewer")

print(sprintf("Loading the folowiong libraries: %s", paste(myLybraries, collapse = ", ")))

for(x in myLybraries) {
  
  # carrega o pacote, se nao existe retorna FALSE
  if (!require(x,character.only = TRUE))
  {
    # instala o pacote
    install.packages(x,dep=TRUE)
    
    # Depois de carregado instala o pacote
    if(!require(x,character.only = TRUE))
      { stop("Package not found") }
  }
} 



