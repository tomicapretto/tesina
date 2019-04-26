# Ultima actualizacion: 31/1/2019
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripcion -------------------------------------------------- #
# Este programa complementa la geodificación inversa realizada en '2. reverse_geocoding.R'.                        #          
# Utilizando la posición de cada vivienda dada por latitud y longitud, se calcula su distancia a cada uno de los   #
# polígonos dentro del Condado de pertenencia.                                                                     #
# Luego, se asigna la vivienda a la localidad que se corresponde con el polígono mas cercano.                      #
#----------------------------------------------------------------------------------------------------------------- #

# Configuracion de working directory.
PATH = "D:/Capre/tesina"
setwd(PATH)

# Carga de librerias.
library(data.table) 
library(sf)
library(lwgeom)

# Lectura de datos.
## Poligonos de localicades.
places <- st_read(dsn = "./datos/source/tx-places/tl_2018_48_place.shp")

## Tabla de viviendas asignadas y no asignadas.
reverse_geocoding <- fread("./datos/bases/reverse_geocoding.csv")
colnames(reverse_geocoding)[colnames(reverse_geocoding) == "place"] <- "place_orig"
reverse_geocoding[reverse_geocoding$place_orig=="",]$place_orig <- NA  # Importados como "" en vez de NA.
reverse_geocoding <- as.data.frame(reverse_geocoding)

points_sf <-  st_as_sf(reverse_geocoding, coords = c("longitude", "latitude"), crs = 4269)

# Asignacion de mismo 'crs' a 'output'. Basado en que ambos sistemas son lo mismo. (ref: 2. reverse_geocoding.R)
st_crs(points_sf) <- st_crs(places)

# Retiene solo las viviendas que no estan contenidas dentro de los polígonos.
# Extrae latitud y longiud de estos poligonos para utilizarlos en la tabla resultante.
# Asigna nombre a columnas.
REM.POINTS <- points_sf[is.na(points_sf$place_orig),]
REM.POINTS <- cbind(REM.POINTS, st_coordinates(REM.POINTS$geometry))
REM.POINTS$place <- NA
REM.POINTS$p_ind <- NA
REM.POINTS <- REM.POINTS[c("sp_id", "hh_race", "hh_income", "county", "place_orig", "place", "p_ind", "X", "Y", "geometry")]
colnames(REM.POINTS) <- c("sp_id", "hh_race", "hh_income", "county", "place_orig", "place", "p_ind", "longitude", "latitude","geometry")

# Retiene solo nombre de indentificacion y geometria de los poligonos.
ALL.POLYGONS <- places[,c("NAME", "geometry")]

# Listado de condados en TX. A utilizar en la iteracion.
COUNTIES <- levels(as.factor(points_sf$county))


# Crea tabla donde se van a guardar los resultados finales. Asigna nombre a las columnas.
assigned.places <- data.frame(matrix(ncol=10, nrow=0))
colnames(assigned.places) <- colnames(REM.POINTS)
assigned.places[,10]<- NULL

# Comienzo de bucle.
start_time <- Sys.time()
for (CT in 1:length(COUNTIES)){

# Selecciona condado y extrae las localidades dentro de ese condado.
  selected.county <- COUNTIES[CT]
  selected.places <- levels(as.factor(points_sf[points_sf$county %in% selected.county,]$place))
  
# Selecciona los puntos y poligonos acorde a condado y localidades.
  selected.points <- REM.POINTS[REM.POINTS$county == selected.county,]
  selected.polygons <- ALL.POLYGONS[ALL.POLYGONS$NAME %in% selected.places,]

# Crea un data.frame local que almacena la informacion de cada condado.
  places.in.iter <- data.frame(matrix(ncol=10, nrow=0))
  colnames(places.in.iter) <- colnames(REM.POINTS)

# El siguiente bucle asigna a cada vivienda (punto) la localidad (poligono) mas proxima, dentro de su condado.
  t1 <- Sys.time()
  for(i in 1:nrow(selected.points)){
    places.in.iter[i, c(1:5,7:10)] <- selected.points[i, c(1:5, 7:10)]
    places.in.iter[i, 6] <- as.character(selected.polygons[which.min(st_distance(selected.polygons, selected.points[i,])),]$NAME)
  }
  
  # Concatenacion de los resultados.
  places.in.iter[,10]<- NULL # Se elimina para no ocupar espacio innecesariamente.
  assigned.places <- rbind(assigned.places, places.in.iter)
  t2 <- Sys.time()  

  print(paste("Time is",t2,"Iteration number", CT, "Iterated for:", as.character.Date(t2-t1), "assignating", nrow(selected.points), "houses"))
}

end_time <- Sys.time()
time.consumed <- end_time - start_time 
time.consumed

# Concatena los resultados con los obtenidos anteriormente y obtiene 
# un dataframe donde todas las viviendas estan asignadas a una ciudad.
# Exporta el resultado a un archivo .csv

reverse_geocoding <- reverse_geocoding[!is.na(reverse_geocoding$place_orig),]
reverse_geocoding$place <- reverse_geocoding$place_orig
reverse_geocoding$p_ind <- "City"
assigned.places$p_ind <- "Rural"

hh_data_2 <- rbind(reverse_geocoding, assigned.places)

fwrite(hh_data_2, "./datos/bases/hh_data_2.csv", sep=",", row.names = FALSE)


