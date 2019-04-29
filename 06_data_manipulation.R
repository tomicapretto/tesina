# Ultima actualizacion: 28/04/2019.
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa utiliza los datos de las viviendas como de las personas.                                           #
# En este se crean las Unidades Primarias de Muestreo (un paso intermedio implica irse a Tableau).                 #
# Realiza filtros del marco muestral de forma tal que este se asimile mas a lo utilizado en un Instituto de        #
# Estadística. Se eliminan las áreas con menos de 50 viviendas y las UPM con menos de 10 areas.                    #
#                                                                                                                  #
# Por otro lado, se crean los totales marginales utilizados para la calibación.                                    #
# También se determinan coeficientes para simular ciertas tasas de respuesta bajo MAR y NMAR.                      #
#----------------------------------------------------------------------------------------------------------------- #

# Configuracion de working directory.
PATH1 = "C:/Users/Tomi/Google Drive/tesina"
setwd(PATH1)

# Carga de librerias
library(data.table)
library(dplyr)

# Lectura de datos de hogares y personas.
hh_data          <- fread("./datos/bases/hh_data_3.csv")
hh_data_filtered <- hh_data[,c("sp_id", "county", "place","locality","area_number")]

ppl_data <- fread("./datos/bases/ppl_data.csv")

# Modificación de clases. Son importados originalmente como 'data.table' y este código utiliza métodos
# que se corresponden con 'data.frame'.
class(hh_data)  <- "data.frame"
class(ppl_data) <- "data.frame"
class(hh_data_filtered) <- "data.frame"

##----------------------------------------------------- Creación de UPM.

# Renombra columnas y anexa Condado a tabla de personas.
ppl_data <- ppl_data %>% 
  rename(person_id = sp_id, sp_id = sp_hh_id) %>%
  left_join(hh_data[c("sp_id","county")], by="sp_id")

# Cantidad de personas y hogares por county.
counties <- ppl_data %>%
  group_by(county) %>%
  summarise(ppl_count = n(), hh_count = n_distinct(sp_id)) %>%
  mutate(county_number = row_number())

# Adiciona Pais y Estado para facilitar geolocalizacion en Tableau.  
counties["Country"] <- "US"
counties["State"]   <- "TX"

# Exporta table... Tableau para agrupar Condados.  
fwrite(counties, "./datos/bases/counties.csv", sep="," ,row.names=FALSE)

#... Luego de agrupacion de Condados.

# Grupos (UPM) formados a partir de los condados a unir. Determinados manualmente con ayuda de Tableau. 
groups_list <- list(c(55, 115, 122), c(9, 40), c(56, 103, 180), c(98, 211), c(106, 148, 197), c(44, 242), c(6,33, 65), c(23, 219), c(38, 96), 
                    c(51, 78, 99, 135, 138), c(77, 173), c(54, 63), c(85, 133), c(76, 104, 217), c(17, 132, 208), c(12, 209, 224), c(61, 140),
                    c(167, 206), c(48, 157, 164), c(69, 136, 193), c(186, 222), c(134, 218), c(53, 118, 207), c(52, 231), c(151, 238, 248),
                    c(88, 196), c(139, 149, 162), c(24, 124, 131), c(41, 168, 216), c(87, 156, 192))

# Asignacion de numero de UPM a uniones de Condados.
for (i in 1:nrow(counties)){
  for (j in 1:length(groups_list)){
    if (counties[i, "county_number"] %in% groups_list[[j]]) {counties[i,"UPM"] <- j}
  }
}

# Asignacion de numero de UPM a Condados que componen una UPM por si solos.
counties <- counties[order(counties$UPM), ]
for(i in 1:nrow(counties)){
  if (is.na(counties[i, "UPM"])) {counties[i, "UPM"] <- as.numeric(counties[i-1, "UPM"]+1) }
}

# Subset y reorden de columnas.
counties <- counties[c("UPM", "county", "ppl_count", "hh_count")]

##----------------------------------------------------- Filtrado de viviendas.
# Pensando en operativo de campo y en mantener UPMs y USMs de forma consistente
# 1- Eliminar areas con < 50 viviendas.
# 2- Eliminar UPM con < 10 areas.

## hh_data tiene 8922361 filas (hogares).
hh_data_filtered <- hh_data_filtered %>%
  left_join(counties, by = "county")
# 1
hh_data_filtered %>%
  group_by(locality, area_number) %>%
  mutate(area_hh_count = n()) %>%
  filter(area_hh_count >=50) %>%
  ungroup() -> hh_data_filtered

# 2
hh_data_filtered %>%
  group_by(UPM, locality) %>%
  mutate(area_count = n_distinct(area_number)) %>%
  filter(area_count >= 10) %>%
  ungroup() %>%
  select(sp_id, UPM) -> hh_data_filtered

# hh_data tiene 8087602 filas (hogares) luego del filtrado.
# Se exporta la tabla con todas las variables, que solo incluye las observaciones
# pertenecientes a areas/UPMs que satisfacen los filtros.
hh_data <- hh_data_filtered %>%
            left_join(hh_data, by = "sp_id")

fwrite(hh_data, "./datos/bases/hh_data_4.csv", sep=",", row.names = FALSE)

##----------------------------------------------------- Elaboracion de tablas de UPM y USM.

UPM_df <- hh_data %>%
  group_by(UPM) %>%
  summarise(hh_count = n()) 

# Estrato de inclusión forzosa.
# UPM = 100, n = 1423955 -> Harris County  (Area metropolitana Houston)
# UPM = 69,  n = 849360  -> Dallas County  (Area metropolitana Dallas)
# UPM = 178, n = 644402  -> Tarrant County (Area metropolitana Forth Worth, pertenece a AM Dallas)
# UPM = 42,  n = 591807  -> Bexar County   (Area metropolitana San Antonio)
# UPM = 183, n = 386352  -> Travis County  (Area metropolitana Austin)
# UPM = 63,  n = 273720  -> Collin County  (Area metropolitana Plano, pertenece a AM Dallas)
# UPM = 78,  n = 247449  -> El Paso County (Area metropolitana El Paso)
#                           Importante por su conexion con NM y Mexico.

UPM_df[UPM_df$hh_count > 245000, "strata"] <- 4
UPM_df[UPM_df$hh_count > 50000 & UPM_df$hh_count <= 245000, "strata"] <- 3
UPM_df[UPM_df$hh_count > 10000 & UPM_df$hh_count <= 50000, "strata"] <- 2
UPM_df[UPM_df$hh_count <= 10000, "strata"] <- 1


USM_df <- hh_data %>%
  group_by(UPM, locality, area_number) %>%
  summarise(hh_count = n())

fwrite(UPM_df , "./datos/bases/UPM_df.csv", sep=",", row.names = FALSE)
fwrite(USM_df , "./datos/bases/USM_df.csv", sep=",", row.names = FALSE)

##----------------------------------------------------- Totales marginales para calibración.

ppl_data$male <- 0
ppl_data[ppl_data$sex == "M", "male"] <- 1
ppl_data$female <- 0
ppl_data[ppl_data$sex == "F", "female"] <- 1

ppl_data$m14 <- 0
ppl_data[ppl_data$age<=14, "m14"] <- 1
ppl_data$m29 <- 0
ppl_data[ppl_data$age>=15 & ppl_data$age<=29, "m29"] <- 1
ppl_data$m49 <- 0
ppl_data[ppl_data$age>=30 & ppl_data$age<=49, "m49"] <- 1
ppl_data$m64 <- 0
ppl_data[ppl_data$age>=50 & ppl_data$age<=64, "m64"] <- 1
ppl_data$m65 <- 0
ppl_data[ppl_data$age>=65, "m65"] <- 1


ppl_data %>%
  group_by(sp_id) %>%
  summarise(n_personas = n(),
            nhh_m14 = sum(m14),
            nhh_m29 = sum(m29),
            nhh_m49 = sum(m49),
            nhh_m64 = sum(m64),
            nhh_m65 = sum(m65),
            nhh_m   = sum(male),
            nhh_f   = sum(female)) %>%
  right_join(hh_data, by ="sp_id") -> hh_data

# Al final no se usa --------------------------------------------------------------------------
# # Se reagrupan los niveles de la variable 'raza del jefe de hogar' para obtener tres dominios
# # de estimacion.Lamentablemente la agrupacion puede no tener sentido en terminos del problema,
# # pero se busca que respondan a tamaños grande, mediano y chico.
# # A que categoria se refiere cada numero puede ser visto en documentacion de la base.
# # 1 - White alone ; 9 - two or more major race groups.
# 
# 
# hh_data[hh_data$hh_race== 1, "hh_race"]       <- 1  # n = 6002786, percent = 74.2%
# hh_data[hh_data$hh_race %in% 2:8, "hh_race" ] <- 2  # n = 1960607, percent = 24.2%
# hh_data[hh_data$hh_race== 9, "hh_race"]       <- 3  # n = 124209,  percent = 1.54%
# 
# # Observar conteos por raza
# hh_data %>%
#   group_by(hh_race) %>%
#   summarise(count_race = n(), percent_race = n()/nrow(.))
# ------------------------------------------------------------------------------------------------

# Se acota superiormente a la cantidad de personas por hogar, luego
# el nivel mas alto se corresponde con viviendas con 5 o mas personas.
# No tiene sentido su valor numerico, sino utilizarlo como categorias.
# Esta variable se utiliza para generar la probabilidad de respuesta en MAR.

hh_data[hh_data$n_personas >=5, "n_personas"] <- 5

# Totales marginales para calibracion.
calibration_margins <- hh_data %>%
  summarise( 
            nhh_m14 = sum(nhh_m14),
            nhh_m29 = sum(nhh_m29),
            nhh_m49 = sum(nhh_m49),
            nhh_m64 = sum(nhh_m64),
            nhh_m65 = sum(nhh_m65),
            nhh_m   = sum(nhh_m),
            nhh_f   = sum(nhh_f))

fwrite(calibration_margins, "./datos/bases/calibration_margins.csv", sep=",", row.names = FALSE)

##----------------------------------------------------- Coeficientes para simular missingness.

# Coeficientes para MAR. 
# Se necesita obtener la frecuencia por nivel de n_personas
# y asignarle a cada nivel una probabilidad de respuesta tal que en el conjunto se obtenga
# una tasa de respuesta deseada (i.e. 0.70).

table(hh_data$n_personas)

# 1       2       3       4       5 
# 1985906 2416863 1350570 1193609 1140654 

# Lo urgente apremia a lo óptimo: la busqueda de coeficientes se hace rudimentariamente en Excel.
# Lista nombrada que almacena vectores con los coeficientes.
MAR_list <- list("0.80" = c(0.69, 0.75, 0.85, 0.90, 0.93),
                 "0.75" = c(0.60, 0.69, 0.83, 0.88, 0.90),
                 "0.70" = c(0.55, 0.65, 0.76, 0.83, 0.85),
                 "0.65" = c(0.52, 0.61, 0.71, 0.73, 0.79),
                 "0.60" = c(0.50, 0.55, 0.62, 0.71, 0.74))

# Coeficientes para NMAR.
# Variable hh_income de nivel alto, muy asimetrica y con valores muy extremos.
# Se estandariza y se trunca.

hh_data$hh_income_std <- scale(hh_data$hh_income)

LI <- -Inf
LS <- -1.0
POINT <- -1.125

while (LS <= 2.50) {
  hh_data[hh_data$hh_income_std >= LI & hh_data$hh_income_std < LS, "hh_income_std" ] <- POINT
  LI <- LS
  LS <- LS + 0.25
  POINT <- POINT + 0.25
}

hh_data[hh_data$hh_income_std >= 2.50 , "hh_income_std"] <- 2.50 

# Al igual que en MAR, se necesita la frecuencia en cada valor para obtener
# los coeficientes asociados a cada tasa de respuesta.
table(hh_data$hh_income_std)

# -1.125  -0.875  -0.625  -0.375  -0.125   0.125   0.375   0.625   0.875   1.125   1.375   1.625 
# 1635 1024340 1706762 1440489 1048235  819253  588453  405921  263265  199556  127237   86835 
# 1.875   2.125   2.375     2.5 
# 69228   44637   36191  225565 

NMAR_list <- list("0.80" = -1.48,
                  "0.75" = -1.16,
                  "0.70" = -0.87,
                  "0.65" = -0.61,
                  "0.60" = -0.37)

##----------------------------------------------------- Tabla final.
hh_data <- hh_data[c("UPM", "locality", "area_number", "sp_id", "hh_income", "hh_income_std", "hh_race", "n_personas",
                     "nhh_m14", "nhh_m29", "nhh_m49", "nhh_m64", "nhh_m65", "nhh_m", "nhh_f")]

#Dataframe definitivo a utilizar durante muestreo y estimación.
fwrite(hh_data,"./datos/bases/hh_data_5.csv", sep=",", row.names = FALSE)










