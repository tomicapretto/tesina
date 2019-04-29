# Ultima actualizacion: 27/04/2019.
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripcion -------------------------------------------------- #
# Este programa realiza geocodificacion inversa de viviendas referenciadas por longitud y latitud.                 #
# Se utiliza la tabla de viviendas del Estado de Texas, junto a poligonos de las localidades de este Estado        #
# provisto por el US Bureau (2018 TIGER/Line® Shapefiles: Places)                                                  #
# Cuando la vivienda esta contenida dentro del poligono, es asignada a esa localidad.                              #
# Cuando ningun poligono contiene a la vivienda, no se produce asignacion.                                         #
# En este ultimo caso, se calcula la distancia de la vivienda a cada uno de los poligonos dentro del condado       #
# y se la asigna a la localidad mas cercana. Se realiza en el programa '03_point_polygon_distance.R'               #
#----------------------------------------------------------------------------------------------------------------- #

# Configuracion de working directory.
PATH1 = "C:/Users/Tomi/Google Drive/tesina"
setwd(PATH1)

# Carga de librerias.
library(readr)
library(sf)

# Lectura de datos.
places <- st_read(dsn = "./datos/source/tx-places/tl_2018_48_place.shp")
st_crs(places)
hh_data <- fread("./datos/bases/hh_data.csv")

# Conversion del data.frame de los datos en un objeto 'sf'.
# Se utiliza el sistema de coordenadas crs=4269 porque es el utilizado en 'places'
hh_data_sf <-  st_as_sf(hh_data, coords = c("longitude", "latitude"), crs = 4269)
st_crs(hh_data_sf)

st_proj_info("datum") # Dado que NAME = GRS80 y NAD83 son lo mismo. Se continua sin problemas.
st_crs(hh_data_sf) <- st_crs(places)

# Realizo 'spatial join' de los puntos en la tabla y los poligonos.
reverse_geocoding <- st_join(hh_data_sf, places['NAME'], join = st_intersects)

## Breve analisis de los resultados.

# 20.32% de las viviendas no fueron asignadas.
# No hay duplicados. :) 
sum(is.na(reverse_geocoding$NAME))/length(reverse_geocoding$NAME) 
nrow(reverse_geocoding) - nrow(hh_data)
sum(duplicated(reverse_geocoding$sp_id)) ; sum(duplicated(reverse_geocoding))

reverse_geocoding <- cbind(reverse_geocoding, st_coordinates(reverse_geocoding$geometry)) 
reverse_geocoding$geometry <- NULL
colnames(reverse_geocoding) <- c("sp_id", "hh_race", "hh_income", "county", "place", "longitude", "latitude")

# Exportar la tabla resultante.
fwrite(reverse_geocoding, "./datos/bases/reverse_geocoding2.csv",sep="," ,row.names=FALSE)

