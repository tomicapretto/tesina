# Ultima actualizacion: 31/1/2019
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripcion -------------------------------------------------- #
# Este programa complementa la geodificación inversa realizada en '02_reverse_geocoding.R'.                        #          
# Utilizando la posición de cada vivienda dada por latitud y longitud, se calcula su distancia a cada uno de los   #
# polígonos dentro del Condado de pertenencia.                                                                     #
# Luego, se asigna la vivienda a la localidad que se corresponde con el polígono mas cercano.                      #
#----------------------------------------------------------------------------------------------------------------- #

# Configuracion de working directory.
PATH1 = "C:/Users/Tomi/Google Drive/tesina"
setwd(PATH1)

# Carga de librerias.
library(data.table) 
library(sf)
library(lwgeom)

# Lectura de datos.
# Poligonos de localicades.
places <- st_read(dsn = "./datos/source/tx-places/tl_2018_48_place.shp")

# Tabla de viviendas asignadas y no asignadas.
reverse_geocoding <- fread("./datos/bases/reverse_geocoding.csv")
colnames(reverse_geocoding)[colnames(reverse_geocoding) == "place"] <- "place_orig"
reverse_geocoding[reverse_geocoding$place_orig=="",]$place_orig <- NA  # Importados como "" en vez de NA.
reverse_geocoding <- as.data.frame(reverse_geocoding)

points_sf <-  st_as_sf(reverse_geocoding, coords = c("longitude", "latitude"), crs = 4269)

# Asignacion de mismo 'crs' a 'output'. Ambos sistemas son lo mismo. (ref: 02_reverse_geocoding.R)
st_crs(points_sf) <- st_crs(places)

# Retiene solo las viviendas que no estan contenidas dentro de los polígonos.
# Extrae latitud y longiud de estos poligonos para utilizarlos en la tabla resultante.
# Asigna nombre a columnas.
rem_points <- points_sf[is.na(points_sf$place_orig),]
rem_points <- cbind(rem_points, st_coordinates(rem_points$geometry))
rem_points$place <- NA
rem_points$p_ind <- NA
rem_points <- rem_points[c("sp_id", "hh_race", "hh_income", "county", "place_orig", "place", "p_ind", "X", "Y", "geometry")]
colnames(rem_points) <- c("sp_id", "hh_race", "hh_income", "county", "place_orig", "place", "p_ind", "longitude", "latitude","geometry")

# Retiene solo nombre de indentificacion y geometria de los poligonos.
all_polygons <- places[, c("NAME", "geometry")]

# Listado de condados en TX a utilizar en la iteracion.
counties <- levels(as.factor(points_sf$county))

# Crea tabla donde se van a guardar los resultados finales. Asigna nombre a las columnas.
assigned_places <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(assigned_places) <- colnames(rem_points)
assigned_places[, 10]<- NULL

# Comienzo de bucle.
start_time <- Sys.time()
for (ct in 1:length(counties)){

  # Selecciona condado y extrae las localidades dentro de ese condado.
  selected_county <- counties[ct]
  selected_places <- levels(as.factor(points_sf[points_sf$county %in% selected_county, ]$place))
  
  # Selecciona los puntos y poligonos acorde a condado y localidades.
  selected_points <- rem_points[rem_points$county == selected_county, ]
  selected_polygons <- all_polygons[all_polygons$NAME %in% selected_places, ]

  # Crea un data.frame local que almacena la informacion de cada condado.
  places_in_iter <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(places_in_iter) <- colnames(rem_points)

  # El siguiente bucle asigna a cada vivienda (punto) la localidad (poligono) mas proxima, dentro de su condado.
  t1 <- Sys.time()
  for(i in 1:nrow(selected_points)) {
    places_in_iter[i, c(1:5,7:10)] <- selected_points[i, c(1:5, 7:10)]
    places_in_iter[i, 6] <- as.character(selected_polygons[which.min(st_distance(selected_polygons, selected_points[i, ])), ]$NAME)
  }
  
  # Concatenacion de los resultados.
  places_in_iter[,10] <- NULL # Se elimina para no ocupar espacio innecesariamente.
  assigned_places <- rbind(assigned_places, places_in_iter)
  t2 <- Sys.time()  

  print(paste("Time is",t2,"Iteration number", ct, "Iterated for:", as.character.Date(t2-t1), "assignating", nrow(selected_points), "houses"))
}

end_time <- Sys.time()
time_consumed <- end_time - start_time 
time_consumed

# Concatena los resultados con los obtenidos anteriormente y obtiene un dataframe donde todas las viviendas estan asignadas a una ciudad.
# Exporta el resultado a un archivo .csv
reverse_geocoding <- reverse_geocoding[!is.na(reverse_geocoding$place_orig), ]
reverse_geocoding$place <- reverse_geocoding$place_orig
reverse_geocoding$p_ind <- "City"
assigned_places$p_ind <- "Rural"

hh_data_2 <- rbind(reverse_geocoding, assigned_places)

fwrite(hh_data_2, "./datos/bases/hh_data_2.csv", sep=",", row.names = FALSE)


