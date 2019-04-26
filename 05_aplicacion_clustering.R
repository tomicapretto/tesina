# Ultima actualizacion: 30/1/2019.
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa utiliza la funcion creada en '4. clustering_function.R' para crear áreas de muestreo               #
# de tamaño similar dentro de cada localidad.                                                                      #
#                                                                                                                  #
# Debido al gran tamaño de la matriz de distancias entre viviendas de una misma localidad, para varias localidades,#
# se opta por dividirlas en porciones de ~8000-12000 viviendas y aplicar el algoritmo de armado de areas de        #
# muestreo dentro de cada una de esas porciones (grand_area). Para ello se utilizo el algoritmo de k-medias        #
# por su simplicidad y velocidad. Este no asegura un tamano uniforme, pero ello no es requerido ya que solo        #
# es un paso intermedio. Basta que lo segmentos producidos sean lo suficientemente mas grandes que el tamaño       #
# deseado para las areas de muestreo (200).                                                                        #
#                                                                                                                  #
# Caracteristicas:                                                                                                 #
# - Se subdivide solamnete a aquellas localidades con mas de 15000 viviendas. Localidades de menor tamaño          #
#   no generan problemas con la memoria RAM (~16GB).                                                               #
# - Cuando la localidad tiene entre 150 y 300 viviendas, el tamaño fijado es de 150 en vez de 200 viviendas.       #
# - Aquellas localidades con menos de 150 viviendas se corresponden con una unica area de muestreo,                #
#   no se utiliza ningun algoritmo de agrupamiento.                                                                #
#                                                                                                                  #
# Luego de limpiar al dataframe de columnas innecesarias, la tabla resultante se exporta en formato .csv           #
#----------------------------------------------------------------------------------------------------------------- #

# Configuracion de working directory.
PATH = "D:/Capre/tesina"
setwd(PATH)

# Carga de librerias y funcion.
library(dplyr)
library(data.table)
source("./codigo/R/4. clustering_function.R")

hh_data <- fread("./datos/bases/hh_data_2.csv")
hh_data$locality <- paste(hh_data$county, hh_data$place, sep="-")

locality_count <- aggregate(sp_id ~ locality, hh_data, FUN=NROW)
names(locality_count)[2] <- "Count"
locality_count <- locality_count[order(-locality_count$Count),]
localities <- locality_count$locality  # Estan ordenadas de mayor a menor.

state_areas <- as.data.frame(matrix(nrow=0, ncol=12))
colnames(state_areas) <- c(colnames(hh_data), "grand_area", "area_hhs")

# Inicializa escritura de archivo de salida.
sink('clustering-output.txt', split=TRUE)

start <- Sys.time()
for (i in 1449:length(localities)){

  iter_locality     <- localities[i]
  locality_hhs <- hh_data[hh_data$locality== iter_locality,]
  
  if (nrow(locality_hhs)>300) {
    if (nrow(locality_hhs)>15000) {
      
      kmeans_size  <- nrow(locality_hhs)%/%10000 + 2
      max_size <- 20000 ; min_size <- 200
      h <- 1
      
      while (max_size>=20000 & min_size<=200){
        kmeans_areas <- kmeans(locality_hhs[,c("latitude", "longitude")], centers = kmeans_size) 
        max_size <- max(kmeans_areas$size)
        min_size <- min(kmeans_areas$size)
        print(paste0("k-means takes ", h, " iterations. Min= ", min_size, ", Max= ", max_size))
        h <- h+1
      }
      
      locality_hhs <- cbind(locality_hhs, kmeans_areas$cluster)
      colnames(locality_hhs)[11] <- "grand_area"  
        
    } else {
      locality_hhs$grand_area <- 1
      kmeans_size             <- 1
    }
      
    locality_areas <- as.data.frame(matrix(nrow=0, ncol=12))
    colnames(locality_areas) <- c(colnames(locality_hhs), "area_hhs")  
    
    for (j in 1:kmeans_size) {
      grand_area_hhs <- locality_hhs[locality_hhs$grand_area==j,]
      area_hhs       <- nnit(grand_area_hhs[,c("latitude", "longitude")], clsize = 200, method='maxd')
      grand_area_hhs <- cbind(grand_area_hhs, area_hhs)
      locality_areas <- rbind(locality_areas, grand_area_hhs, fill=TRUE)
      print(paste0("Finished grand area ", j, " n=", nrow(grand_area_hhs), " in ", iter_locality ,
                   " at ", Sys.time(), ". ", kmeans_size-j," area/s remaining"))
    }
     
  } else if (nrow(locality_hhs)>150) {
    
    locality_hhs$grand_area <- 1
    area_hhs <- nnit(locality_hhs[,c("latitude", "longitude")], clsize = 150, method='maxd')
    locality_areas  <- cbind(locality_hhs, area_hhs)
    
  } else {  
    
    locality_hhs$grand_area <- 1
    locality_hhs$area_hhs   <- 1
    locality_areas <- locality_hhs
  } 
  
  state_areas <- rbind(state_areas, locality_areas)
  print(paste0("Finished ", iter_locality, " n=", nrow(locality_hhs) ," at ", Sys.time()))
  print(paste0("Next locality is ", localities[i+1]))

}

end <- Sys.time()
end-start

# Finaliza escritura de archivo de salida.
sink('clustering-output.txt', append=FALSE)
sink()

# Backup por si las dudas.
fwrite(state_areas, "./datos/bases/state_areas.csv", sep=",", row.names=FALSE)

# Asigna numeracion a las areas dentro de cada localidad.
hh_data_3 <- state_areas %>%
              group_by(locality) %>%
              mutate(area_number= as.integer(interaction(grand_area, area_hhs, drop=TRUE)))

hh_data_3[c("grand_area","area_hhs")] <- list(NULL)

# Exporta tabla de datos.
fwrite(hh_data_3, "./datos/bases/hh_data_3.csv", sep=",", row.names=FALSE)



