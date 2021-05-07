# c("sp", "ggmap", "tidyverse", "magrittr")
# Carrega blibliotecas que sao usadas
# library(sp)
# library(ggmap)
# library(dplyr)
# library(magrittr)

# library(gstat)
source("./InstallPackages.R")


# Carrega os dados
data(meuse)

# Transforma meuse de "data.frame" para "SpatialPointsDataFrame" class
sp::coordinates(meuse) = ~x+y

class(meuse)

summary(meuse)


# To save a plot, you need to do the following:
# Open a device, using png(), bmp(), pdf() or similar
# Plot your model
# Close the device using dev.off()
# Some example code for saving the plot to a png file:



rdh_coords <- meuse %>% as.data.frame %>% dplyr::select(x, y, zinc)
sp::coordinates(rdh_coords) <- ~x + y
sp::proj4string(rdh_coords) <- sp::CRS("+init=epsg:28992")
longlat_coords <- rdh_coords %>% sp::spTransform(sp::CRS("+init=epsg:4326"))


meuse.df <- as.data.frame(longlat_coords)

# plot(meuse)
ggplot(meuse.df, aes(x,y)) + geom_point(shape = 4) + coord_equal()
ggsave("plot.png")

ggplot(meuse.df, aes(x,y)) + geom_point(aes(size = zinc)) + coord_equal()


# api <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
# register_google(key = api)
# mapa.meuse <- get_map(bbox(longlat_coords), source = "google", maptype="satellite")
# ggmap(mapa.meuse) +
#   geom_point(aes(x,y, size = zinc, colour = "white"), data = meuse.df) +
#   coord_equal()



# carrega os dados
data(meuse.grid)

# Transforma meuse.grid de "data.frame" para "SpatialPointsDataFrame" class
sp::coordinates(meuse.grid) = ~x+y

# Transforma meuse.grid de "data.frame" para "SpatialPixelsDataFrame" class
sp::gridded(meuse.grid) = TRUE

# Verifica a classe do objeto
class(meuse.grid)

# Poderação pelo inverso da distância (IDW)
zinc.idw <- gstat::idw(zinc~1, meuse, meuse.grid)

str(zinc.idw)

spplot(zinc.idw["var1.pred"], main = "interpolacao pelo inverso da distância")
