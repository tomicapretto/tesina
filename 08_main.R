if (iter <= 1000) estimate_var = TRUE else estimate_var = FALSE  # determina si se calculan variancias o no.
## -------------------------------------------- 1 - Selección de muestra compleja.
print(paste0(Sys.time(), ": Selecting sample"))

muestra <- select_sample(UPM_df = UPM_data, USM_df = USM_data, UTM_df = hh_data,
                         UPM_n = ET1_n, USM_n = ET2_n, UTM_n = ET3_n, strata = strata)

USM_sample <- muestra$USM_sample; UTM_sample <- muestra$UTM_sample

# Para estimación de variancia mediante bootstrap RWY.
# Estrato no auto-representado: UPM_boot = UPM
# Estrato auto-representado:    UPM_boot = USM
USM_sample$UPM_boot    <- ifelse(USM_sample$strata == 4, paste0(USM_sample$locality, "-" , USM_sample$area_number), as.character(USM_sample$UPM))
USM_sample$strata_boot <- ifelse(USM_sample$strata == 4, USM_sample$UPM, USM_sample$strata)
USM_sample$f_boot      <- ifelse(USM_sample$strata == 4, USM_sample$USM_f, USM_sample$UPM_f )

UTM_sample <- UTM_sample %>%
  left_join(USM_sample[c("locality", "area_number", "UPM_boot", "strata_boot", "f_boot")], 
            by = c("locality", "area_number")) 

sim_data <- UTM_sample %>%
  select(hh_income, hh_income_std, n_personas, nhh_m14, nhh_m29, nhh_m49, nhh_m64, nhh_m65,nhh_f, nhh_m,
         strata, wt, UPM_boot, strata_boot, f_boot)

rm(muestra, UTM_sample, USM_sample)  

## -------------------------------------------- 2 - Creación de objeto de simulacion.
sim_object <- init_object()
sim_object$variables <- sim_data
sim_object$pweights$base <- sim_data$wt
sim_object$strata_fpc <- sim_data$f_boot

## -------------------------------------------- 3 - Creación de pesos de replicación.
if (estimate_var){
  sim_object$repweights$base <- get_bootweights(sim_object$variables$strata_boot,
                                              sim_object$variables$UPM_boot,
                                              sim_object$variables$wt, 
                                              replicates = B,
                                              usefpc = fpc_status,
                                              fpc = sim_object$strata_fpc)
}

## -------------------------------------------- 4 - Asignación de missings.
# No se eliminan los registros que no responden porque se los utiliza para ajustar los pesos bootstrap por NR.
# Cantidad de respondientes variable -> La respuesta es un fenomeno estocastico.
print(paste0(Sys.time(), ": Assigning missing values"))

list_sim_object <- list()
list_sim_object$MCAR <- list_sim_object$MAR <- list_sim_object$NMAR <- sim_object

list_sim_object$MCAR$I <- rbinom(n = nrow(sim_object$variables), size = 1, prob = as.numeric(TASA))

list_sim_object$MAR$I <- sim_object$variables %>%
  mutate(I = rbinom(n = n(), size=1, prob = MAR_list[[TASA]][n_personas])) %>%
  select(I) %>% unlist() %>% unname()

list_sim_object$NMAR$I <- sim_object$variables %>%
  mutate(prob_resp = 1/(1+exp(NMAR_list[[TASA]] + hh_income_std)),
         I = rbinom(n = n(), size = 1, prob = prob_resp)) %>%
  select(I) %>% unlist() %>% unname()
  
## -------------------------------------------- 5 - Ajustes por no respuesta.
print(paste0(Sys.time(), ": No response adjustments..."))
if (estimate_var) { 
  cl <- makeCluster(detectCores()-3)
  clusterExport(cl, c("sub_adjust_noresp"))
  registerDoParallel(cl)
  print(paste0(Sys.time(),": Cluster initialized."))
}

list_sim_object$MCAR <- adjust_noresp(list_sim_object$MCAR, estimate_var)
print(paste0(Sys.time(), ": MCAR Finished"))

list_sim_object$MAR <- adjust_noresp(list_sim_object$MAR, estimate_var)
print(paste0(Sys.time(), ": MAR Finished"))

list_sim_object$NMAR <- adjust_noresp(list_sim_object$NMAR, estimate_var)
print(paste0(Sys.time(), ": NMAR Finished"))

## -------------------------------------------- 6 - Calibración pesos.
# Este bloque realiza la calibración de los pesos muestrales y pesos bootstrap.
print(paste0(Sys.time(), ": Calibrating..."))

list_sim_object$MCAR <- do_calibrate(list_sim_object$MCAR, poptotals = calib_totals, Xvars = calib_vars, estimate_var)
print(paste0(Sys.time(), ": Calibration - MCAR Finished"))

list_sim_object$MAR <- do_calibrate(list_sim_object$MAR, poptotals = calib_totals, Xvars = calib_vars, estimate_var)
print(paste0(Sys.time(), ": Calibration - MAR Finished"))

list_sim_object$NMAR <- do_calibrate(list_sim_object$NMAR, poptotals = calib_totals, Xvars = calib_vars, estimate_var)
print(paste0(Sys.time(), ": Calibration - NMAR Finished"))

if (estimate_var) {
  stopCluster(cl)
  print(paste0(Sys.time(),": Cluster stopped."))
}

## -------------------------------------------- 6 - Estimación.
print(paste0(Sys.time(), ": Estimation stage"))

sim_output <- list(
  "MCAR" = estimate(list_sim_object$MCAR, estimate_var),
  "MAR"  = estimate(list_sim_object$MAR, estimate_var),
  "NMAR" = estimate(list_sim_object$NMAR, estimate_var)
)

for (nr in c("MCAR", "MAR", "NMAR")){
  resultado[[nr]][["medias"]] <- rbind(resultado[[nr]][["medias"]], 
                                       as.numeric(sim_output[[nr]][["medias"]]))
  
  if (estimate_var){
    resultado[[nr]][["variancias"]]   <- rbind(resultado[[nr]][["variancias"]], 
                                               as.numeric(sim_output[[nr]][["variancias"]]))
  }
}

gc()
print(paste0(Sys.time(), ": Finished."))



