# Ultima actualizacion: 3/1/2019.
# Autor: Tomas Capretto.
#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa lista, concatena y escribe todos los archivos dentro de la carpeta TX que refieren a los hogares   #
# y personas.                                                                                                      #
# El nivel geográfico de cada archivo es el Condado.                                                               #
# El resultado de este programa son dos archivos con formato .csv                                                  #
#                                                                                                                  #
# El primero contiene a los hogares con las siguientes variables:                                                  #
# (*) Código único de identificación de hogar (sp_id)                                                              #
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
PATH = "D:/Capre/tesina"
setwd(PATH)
library(data.table) # Mas rapido que {base} y {readr}, sin corromper archivos.

## Hogares
FILES.HH <- list.files(path = "./datos/source/TX/" ,pattern = "households.txt",recursive = TRUE)
FILES.HH <- paste0('./datos/source/TX/', FILES.HH)

FILES.HH.METADATA <- list.files(path = "./datos/source/TX/", pattern = "METADATA.txt",recursive = TRUE)
FILES.HH.METADATA <- paste0('./datos/source/TX/', FILES.HH.METADATA)

HH.FULL <- data.frame()
for (i in 1:length(FILES.HH)) {
  
  ingestion <- fread(FILES.HH[i])
  string <- readLines(FILES.HH.METADATA[i])
  county <- sub(".*?geography name: (.*?), Texas.*", "\\1", string[51])
  ingestion$county <- county
  HH.FULL <- rbind(HH.FULL, ingestion)
}

HH.Data <- HH.FULL[,c("sp_id","latitude","longitude","hh_race","hh_income","county")]
fwrite(HH.Data,"./datos/bases/hh_data.csv",sep = "," ,row.names = FALSE) #much faster than write.csv

## Personas 
# '^people.txt' solo selecciona a los que comienzan con 'people.txt'
FILES.PPL <- list.files(path = "./datos/source/TX/",pattern = "^people.txt",recursive = TRUE)
FILES.PPL <- paste0('./datos/source/TX/', FILES.PPL)

PPL.FULL <- do.call(rbind, lapply(FILES.PPL, fread))

PPL.Data <- PPL.FULL[,c("sp_id","sp_hh_id","age","sex","race")]
fwrite(PPL.Data,"./datos/bases/ppl_data.csv",sep = "," , row.names = FALSE)

