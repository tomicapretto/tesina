PATH1 = "C:/Users/Tomi/Google Drive/tesina"
PATH2 = "D:/Capre/tesina"
PATH3 = "C:/Users/Capre/Desktop/Google Drive/tesina"
setwd(PATH1)

set.seed(121195)

# Lectura librerias y funciones propias.
library(sampling)
library(dplyr)
library(data.table)
library(foreach)     # computación paralela.
library(doParallel)  # computación paralela.
source("./codigo/R/07_utils.R")

# Lectura datos y conversion a clase data.frame.
UPM_data <- fread("./datos/bases/UPM_df.csv")
USM_data <- fread("./datos/bases/USM_df.csv")
hh_data  <- fread("./datos/bases/hh_data_5.csv")
calib_totals <- fread("./datos/bases/calibration_margins.csv")

class(UPM_data) <- "data.frame"
class(USM_data) <- "data.frame"
class(hh_data)  <- "data.frame"
class(calib_totals) <- "data.frame"

# Tamanos muestrales.
ET1_n <- c(30, 20, 8, 7) 
ET2_n <- c(10, 30, 60, 250) 
ET3_n <- 10

TASA = "0.80"
B = 200
niter = 10000
fpc_status = FALSE
calib_vars <- c("nhh_m14", "nhh_m29", "nhh_m49", "nhh_m64", "nhh_m65", "nhh_m", "nhh_f")

#"sin ajuste", "ajustado por NR1", "ajustado por NR2", "ajustado por calibracion", "ajustado por NR1+cal", "ajustado por NR2+cal"
resultado <- list(
  "MCAR" = list("medias" = matrix(ncol=6, nrow=0), "variancias" = matrix(ncol=6, nrow=0)),
  "MAR"  = list("medias" = matrix(ncol=6, nrow=0), "variancias" = matrix(ncol=6, nrow=0)),
  "NMAR" = list("medias" = matrix(ncol=6, nrow=0), "variancias" = matrix(ncol=6, nrow=0))
)

for (iter in 1:niter){
  
  print(paste0(Sys.time(),": Iteration number ", iter))
  source("./codigo/R/08_main.R")
  
  if (iter%%500==0){
    print(paste0(Sys.time(),": Iteration number ", iter, ". Saving to external files"))
    for(c in c("MCAR", "MAR", "NMAR")) {
      
      if (estimate_var) measurements <- c("medias" , "variancias") else measurements <- "medias"
      
      for (d in measurements) {
        
        fwrite(as.data.frame(resultado[[c]][[d]]), 
               paste0("./datos/sim_output/result_80_", c, d, ".csv"), sep=",", row.names=FALSE)
      }
    }
  }
}
