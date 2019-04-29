# Ultima actualizacion: 27/04/2019.
# Autor: Tomas Capretto.
#-------------------------------------------------- Descripcion -------------------------------------------------- #
# Este programa lista, concatena y escribe todos los archivos dentro de la carpeta TX que refieren a los hogares   #
# y personas.                                                                                                      #
# El nivel geografico de cada archivo es el Condado.                                                               #
# El resultado de este programa son dos archivos con formato .csv                                                  #
#                                                                                                                  #
# El primero contiene a los hogares con las siguientes variables:                                                  #
# (*) Codigo unico de identificacion de hogar (sp_id)                                                              #
# (*) Latitud (latitud)                                                                                            #
# (*) Longitud (longitud)                                                                                          #
# (*) Raza del jefe de hogar (hh_race)                                                                             #
# (*) Ingreso del hogar (hh_income)                                                                                #
# (*) Condado donde se encuentra el hogar (county)                                                                 #
#                                                                                                                  #
# El segundo contiene a los individuos y las siguientes variables:                                                 #
# (*) Codigo unico de identificacion de la persona (sp_id)                                                         #
# (*) Codigo unico de identificacion de hogar (sp_hh_id)                                                           #
# (*) Edad de la persona (age)                                                                                     #
# (*) Sexo de la persona (sex)                                                                                     #
# (*) Raza de la persona (race)                                                                                    #
#----------------------------------------------------------------------------------------------------------------- #

# Configuracion de working directory.
PATH1 = "C:/Users/Tomi/Google Drive/tesina"
setwd(PATH1)

# Carga de librerias.
library(data.table)

# Lectura de archivos con datos referentes a hogares.
files_hh <- list.files(path = "./datos/source/TX/", pattern = "households.txt", recursive = TRUE)
files_hh <- paste0('./datos/source/TX/', files_hh)

files_hh_metadata <- list.files(path = "./datos/source/TX/", pattern = "METADATA.txt", recursive = TRUE)
files_hh_metadata <- paste0('./datos/source/TX/', files_hh_metadata)

hh_full <- data.frame()
for (i in 1:length(files_hh)) {
  
  ingestion <- fread(files_hh[i])
  string <- readLines(files_hh_metadata[i])
  county <- sub(".*?geography name: (.*?), Texas.*", "\\1", string[51])  #  obtiene nombre del county.
  ingestion$county <- county
  hh_full <- rbind(hh_full, ingestion)
  
}

# Selección de variables y escritura de archivo '.csv' con datos de hogares.
hh_data <- hh_full[, c("sp_id", "latitude", "longitude", "hh_race", "hh_income", "county")]
fwrite(hh_data,"./datos/bases/hh_data.csv", sep = ",", row.names = FALSE)

# Lectura de archivos con datos referentes a personas.
files_ppl <- list.files(path = "./datos/source/TX/",pattern = "^people.txt",recursive = TRUE)
files_ppl <- paste0('./datos/source/TX/', files_ppl)

ppl_full <- do.call(rbind, lapply(files_ppl, fread))

# Selección de variables y escritura de archivo '.csv' con datos de personas.
ppl_data <- ppl_full[,c("sp_id", "sp_hh_id", "age" ,"sex", "race")]
fwrite(ppl_data,"./datos/bases/ppl_data.csv", sep = ",", row.names = FALSE)

