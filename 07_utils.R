# Ultima actualizacion: 28/03/2019.
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa contiene una función utilizada para extraer una muestra por conglomerados estratificada,           #
# en 3 etapas, donde la ultima unidad de seleccion son elementos.                                                  #
# El resultado es una lista que contiene 3 dataframes, cada uno se corresponde con la muestra de cada              #
# etapa de muestreo.                                                                                               #
#----------------------------------------------------------------------------------------------------------------- #

## -------------------------------------------- Función para seleccionar muestra.
# UPM_df: data.frame de UPM (Estrato + UPM + size_variable -hh_count- )
# USM_df: data.frame de USM (UPM + locality + area_number + size_variable -hh_count- )
# UTM_df: data.frame de UTM (locality + area_number + variables_interes)
# UPM_n:  vector de tamaños muestrales de longitud igual a cantidad de estratos.
# USM_n:  vector de tamaños muestrales de longitud igual a cantidad de estratos.
# UTM_n:  cantidad de viviendas a seleccionar en la ultima etapa.
# strata: variable de estratificación.

select_sample <- function(UPM_df, USM_df, UTM_df, UPM_n, USM_n, UTM_n, strata){ 
  
  # Etapa 1: Probabilidad proporcional al tamaño. Sistemático Madow.
  UPM_data %>%
    filter(strata != 4) %>%
    mutate(UPM_n = UPM_n[strata]) %>%
    group_by(strata) %>%
    mutate(UPM_pik = inclusionprobabilities(hh_count, UPM_n),
           UPM_I   = UPsystematic(UPM_pik, eps = 1e-7),
           UPM_f   = sum(UPM_I)/n() ) %>%  # fracción de muestreo de UPM.
    filter(UPM_I == 1) %>% 
    ungroup() %>%
    select(strata, UPM, UPM_pik, UPM_f) %>%
    rbind(cbind(UPM_data[UPM_data$strata == 4, c("strata", "UPM")], "UPM_pik" = 1, "UPM_f" = 1)) -> 
    UPM_sample
  
  # Etapa 2: Muestreo simple al azar
  USM_data %>%
    right_join(UPM_sample, by = "UPM") %>%
    mutate(USM_n = USM_n[strata]) %>%
    group_by(UPM) %>%
    mutate(USM_pik = USM_n/n(),
           USM_I   = srswor(USM_n, n()),
           USM_f   = sum(USM_I)/n()) %>%
    filter(USM_I == 1) %>%
    ungroup() %>%
    select(strata, UPM, locality, area_number, UPM_pik, UPM_f, USM_pik, USM_f) -> 
    USM_sample
  
  # Etapa 3: Muestreo simple al azar sin reemplazo
  UTM_df %>%
    right_join(USM_sample, by = c("UPM","locality", "area_number")) %>%
    group_by(locality, area_number) %>%
    mutate(UTM_pik = UTM_n/n(),
           UTM_I   = srswor(UTM_n, n()),
           pik     = UPM_pik*USM_pik*UTM_pik,
           wt      = 1/pik) %>%
    filter(UTM_I == 1) %>%
    select(-c(UPM_pik, USM_pik, UTM_pik, UTM_I, UPM_f, USM_f)) %>%
    ungroup() -> 
    UTM_sample

  return(list("UPM_sample" = UPM_sample,"USM_sample" = USM_sample ,"UTM_sample" = UTM_sample))
}  

## -------------------------------------------- Función para realizar ajuste por no respuesta.
# object:       Objecto de simulación inicializado con 'init_object'. 
# estimate_var: booleano que indica si se cuenta con pesos de replicacion para estimar variancia

adjust_noresp <- function(object, estimate_var) {

# Ajusto pesos de diseño.
  # Clase de ajuste: numero de personas en el hogar.  Retiene solo los respondientes
  object$pweights$adj1 <- sub_adjust_noresp(weights = object$pweights$base,
                                            group = object$variables$n_personas,
                                            resp = object$I)
  
  # Clase de ajuste: estrato de UPM. Retiene solo los respondientes
  object$pweights$adj2 <- sub_adjust_noresp(weights = object$pweights$base,
                                            group = object$variables$strata,
                                            resp = object$I)
  
  object$pweights$base <- object$pweights$base[object$I == 1]
    
# cl <- makeCluster(detectCores()-2); registerDoParallel(cl); s <- Sys.time()

  if (estimate_var){  # Solo se calcula la variancia para 1000 iteraciones.
    # Ajusto pesos de replicacion.  
    # Loop paralelo que realiza ambos ajustes en pesos de replicación.
    adj_boot <- foreach(i = 1:ncol(object$repweights$base), .combine = 'combine', .multicombine = TRUE, 
                        .packages = c('foreach')) %dopar% {
      
      res1 <- sub_adjust_noresp(weights = object$repweights$base[, i],
                                group = object$variables$n_personas,
                                resp = object$I)   
      
      res2 <- sub_adjust_noresp(weights = object$repweights$base[, i],
                                group = object$variables$strata,
                                resp = object$I)
      list(res1, res2)
    }
    
    object$repweights$base <- object$repweights$base[object$I == 1,] # Me quedo solo con respondientes.
    object$repweights$adj1 <- adj_boot[[1]]                          # a los demas le vuelven solo los respondientes.
    object$repweights$adj2 <- adj_boot[[2]]
  }
  
  object$variables <- object$variables[object$I == 1,] # Solo respondientes. 
  object$strata_fpc <- object$strata_fpc[object$I == 1]
  
# stopCluster(cl); e <- Sys.time(); e -s
  return(object)
}


# Otra alternativa para el loop paralelo. Testearlo en la PC de Blas. (future_apply)
#
# apply(X = object$repweights$base, 2, function (x) sub_adjust_noresp(weights = x,
#                                                                     group = object$variables$strata,
#                                                                     resp = object$I))

## -------------------------------------------- Funcion auxiliar para el ajuste por no respuesta.
# weights: vector con los pesos a ajustar
# group:   vector con la variable de clasificacion
# resp:    vector con la variable indicadora de respuesta.

sub_adjust_noresp <- function(weights, group, resp){
  
  adjweights <- weights
  group <- as.character(group)
  
  for (g in unique(group)) {
    num <- tapply(weights, group, sum)
    den <- tapply(weights[resp == 1], group[resp == 1], sum)
    fadj <- num/den
    
    adjweights[group == g] <- fadj[g]*weights[group == g]
  }
  return(adjweights[resp == 1])
}

## -------------------------------------------- Función para realizar calibración.
# object:       Objecto de simulación inicializado con 'init_object'.
# poptotals:    Vector de totales poblacionales.
# Xvars:        Vector caracter de variables de calibracion.
# estimate_var: booleano que indica si se cuenta con pesos de replicacion para estimar variancia

do_calibrate <- function(object, poptotals, Xvars, estimate_var = TRUE) {        

  sample_Xs <- as.matrix(object$variables[Xvars])

  # Ajusto pesos de diseño.
  g_wt  <- calib(Xs = sample_Xs, d = object$pweights$base, total = as.numeric(poptotals), method = "logit", bounds = c(0.4, 3.85))
  object$pweights$cal <- g_wt*object$pweights$base
  
  g_wt  <- calib(Xs = sample_Xs, d = object$pweights$adj1, total = as.numeric(poptotals), method = "logit", bounds = c(0.4, 3.85))
  object$pweights$cal_adj1 <- g_wt*object$pweights$adj1
  
  g_wt  <- calib(Xs = sample_Xs, d = object$pweights$adj2, total = as.numeric(poptotals), method = "logit", bounds = c(0.4, 3.85))
  object$pweights$cal_adj2 <- g_wt*object$pweights$adj2
  
  # cl <- makeCluster(detectCores()-2); registerDoParallel(cl); s <- Sys.time()
  if (estimate_var){  # Solo se calcula la variancia para 1000 iteraciones.
    # Ajusto pesos de calibracion.
    calibrated_wt <- foreach(i = 1:ncol(object$repweights$base), .combine = 'combine', .packages=c('sampling', 'foreach')) %dopar% {
      
      initial_wt <- as.vector(object$repweights$base[, i])
      g_wt  <- calib(Xs = sample_Xs, d = initial_wt, total = as.numeric(poptotals), method = "logit", bounds = c(0.4, 3.85))
      res1  <- g_wt*initial_wt
      
      initial_wt <- as.vector(object$repweights$adj1[, i])
      g_wt  <- calib(Xs = sample_Xs, d = initial_wt, total = as.numeric(poptotals), method = "logit", bounds = c(0.4, 3.85))
      res2  <- g_wt*initial_wt
      
      initial_wt <- as.vector(object$repweights$adj2[, i])
      g_wt  <- calib(Xs = sample_Xs, d = initial_wt, total = as.numeric(poptotals), method = "logit", bounds = c(0.4, 3.85))
      res3  <- g_wt*initial_wt
      
      list(res1, res2, res3)
    }
  
    object$repweights$cal      <- calibrated_wt[[1]]
    object$repweights$cal_adj1 <- calibrated_wt[[2]]
    object$repweights$cal_adj2 <- calibrated_wt[[3]]
  }
  # stopCluster(cl); e <- Sys.time(); e -s
  return(object) 
}

## -------------------------------------------- Funcion que calcula pesos de replicacion.
# strata:     vector de estrato de unidad primaria de muestreo
# psu:        vector que indica la unidad primaria de muestreo
# dweights:   vector de pesos de diseno
# replicates: cantidad de replicas bootstrap a obtener
# usefpc:     booleano que determina si se usa la fraccion de muestreo por estrato bootstrap o no
# fpc:        fraccion de muestreo por estrato bootstrap

get_bootweights <- function(strata, psu, dweights, replicates = 50, usefpc = FALSE, fpc = NULL){
  
  index   <- match(psu, psu[!duplicated(psu)])
  upsu    <- unique(psu)
  strata  <- as.character(strata)
  weights <- matrix(nrow = length(upsu), ncol = replicates)
  ustrata <- strata[!duplicated(psu)]
 
  for (s in unique(ustrata)) {
    this.stratum <- ustrata == s
    npsu <- length(unique(upsu[this.stratum])) # Para que esta esto? Tambien en 'survey'
    weights[this.stratum, ] <- get_psuweight(upsu[this.stratum], replicates)
  }
  
  # Devuelve una matriz con los pesos de replicacion  de 'analysis'
  repweights <- weights[index, ]
  
  if (usefpc) repweights <- 1 + sqrt(1 - fpc) * (repweights - 1)
  
  as.matrix(repweights)*(dweights)
}

## -------------------------------------------- Función que calcula factor de corrección para bootstrap por estrato. 
# psu: vector que indica la unidad primaria de muestreo
# replicates: cantidad de replicas bootstrap a obtener
get_psuweight <- function(psu, replicates) {
  
  upsu <- sample(unique(psu))
  n <- length(upsu)

  replicate(replicates,
            table(factor(sample(upsu, length(upsu)-1, replace=TRUE), 
                         levels = unique(psu))))*n/(n-1)
}

## -------------------------------------------- Función que inicializa objeto de simulación.
init_object <- function(){
  
  list("variables" = NULL, 
       "pweights"   = list(),
       "repweights" = list(),
       "strata_fpc" = NULL,
       "I" = NULL)
}

## -------------------------------------------- Función que obtiene multiple resultados de foreach
combine <- function(...) {
  mapply('cbind', ..., SIMPLIFY=FALSE)
}



## -------------------------------------------- Función que realiza estimaciones puntuales y de variancia.
estimate <- function(object, estimate_var){
  
  estimate_mean <- function(weights, y_vals) { # Estimador de Hajek.
    sum(weights * y_vals) / sum(weights)
  } 
  
  y <- object$variables$hh_income
  y_means <- sapply(object$pweights, estimate_mean, y)
  
  if (estimate_var){
    # Para cada conjunto de pesos de replicación calcula el estimador de Hajek
    # en cada vector de pesos. Luego calcula la variancia bootstrap utilizando estas medias.
    
    y_vars  <- apply(sapply(object$repweights, function (x) apply(x, 2, estimate_mean, y)), 2, var)
    return(list("medias" = y_means, "variancias" = y_vars))
  } else {
    return(list("medias" = y_means))
  }
}  


## -------------------------------------------- Listas con coeficientes para asignación de missings.
MCAR_list <- list("0.80" = 0.80,
                  "0.75" = 0.75,
                  "0.70" = 0.70,
                  "0.65" = 0.65,
                  "0.60" = 0.60)

MAR_list <- list("0.80" = c(0.69, 0.75, 0.85, 0.90, 0.93),
                 "0.75" = c(0.60, 0.69, 0.83, 0.88, 0.90),
                 "0.70" = c(0.55, 0.65, 0.76, 0.83, 0.85),
                 "0.65" = c(0.52, 0.61, 0.71, 0.73, 0.79),
                 "0.60" = c(0.50, 0.55, 0.62, 0.71, 0.74))

NMAR_list <- list("0.80" = -1.48,
                  "0.75" = -1.16,
                  "0.70" = -0.87,
                  "0.65" = -0.61,
                  "0.60" = -0.37)









#------------------------------------ Deprecated ------------------------------------------- ###

## -------------------------------------------- Función para obtener pesos bootstrap, ajustados y no ajustados por NR.

# nh = cantidad de UPM del estrato h en la muestra.
# mh = cantidad de UPM del estrato h a seleccionar con reposicion (i.e. mh=nh-1)
# thi = cantidad de veces que sale UPM i del estrato h en la muestra con reposicion.

# boot_adj <- function(UPM_df,      # data.frame con la muestra de UPMs a utilizar.
#                      UTM_df,      # data.frame con la muestra de elementos a utilizar.
#                      UPM_n,       # vector que indica cantidad de UPM por estrato en la muestra.
#                      boot_mh,     # vector que indica cantidad de UPM por estrato en cada muestra bootstrap.
#                      n_boot       # cantidad de replicas bootstrap.
# ){  
#   # Dataframe utilizado para asistir la obtencion de pesos bootstrap y bootstrap ajustados por NR.
#   input_data <- UTM_df[c("strata", "strata_boot", "UPM_boot", "wt", "n_personas" ,"I")]
#   
#   # Obtiene pesos bootstrap y pesos bootstrap ajustados por NR, utilizando primero 
#   # "Adj.Class" como clase de ajuste de pesos.
#   boot_wt <- foreach(i=1:n_boot, .combine=cbind, .packages=c('sampling','dplyr', 'foreach')) %dopar% {
#     
#     oldnames = c("boot_wt","adj1_boot_wt", "adj2_boot_wt")
#     newnames = c(paste0("boot_wt_", i), paste0("adj1_boot_wt_", i), paste0("adj2_boot_wt_", i))
#     
#     boot_fact <- cbind(UPM_df %>%
#                          arrange(strata_boot, UPM_boot) %>%
#                          select(strata_boot, UPM_boot),
#                        thi = unlist(mapply(srswr, boot_mh, UPM_n))) %>% 
#       group_by(strata_boot) %>%
#       mutate(num = n(), den = sum(thi)) %>%
#       ungroup() %>%
#       mutate(adj_factor = thi*(num/den)) %>%
#       select(strata_boot, UPM_boot, adj_factor)
#     
#     # Ajuste NR por clases dada por cantidad de personas en el hogar. Para ver MAR.
#     input_data %>%
#       left_join(boot_fact, c("strata_boot", "UPM_boot"))%>%
#       mutate(boot_wt = wt*adj_factor) %>%
#       group_by(n_personas) %>% 
#       mutate(tot_boot_wt = sum(boot_wt)) %>%
#       filter(I == 1) %>%
#       mutate(adj_nr       = tot_boot_wt/sum(boot_wt),
#              adj1_boot_wt = adj_nr*boot_wt) %>%
#       ungroup() %>%
#       rename_at(vars(oldnames[1:2]), ~ newnames[1:2]) %>% 
#       select(newnames[1:2]) -> part1
#     
#     # Ajuste NR por clases por estrato. Similar a la realidad, para ver que pasa.
#     input_data %>%
#       left_join(boot_fact, c("strata_boot", "UPM_boot"))%>%
#       mutate(boot_wt = wt*adj_factor) %>%
#       group_by(strata) %>% 
#       mutate(tot_boot_wt = sum(boot_wt)) %>%
#       filter(I == 1) %>%
#       mutate(adj_nr       = tot_boot_wt/sum(boot_wt),
#              adj2_boot_wt = adj_nr*boot_wt) %>%
#       ungroup() %>%
#       rename_at(vars(oldnames[3]), ~ newnames[3]) %>% 
#       select(newnames[3]) -> part2
#     
#     b <- cbind(part1, part2)
#   }
#   
#   # Agrega pesos ajustados por no respuesta, utilizando n_personas como clase de ajuste.
#   boot_output1 <- input_data %>% 
#     group_by(n_personas) %>%
#     mutate(tot_wt = sum(wt)) %>%
#     filter(I == 1) %>%
#     mutate(adj_nr = tot_wt/sum(wt),
#            adj1_wt = adj_nr*wt) %>%
#     ungroup() %>%
#     select(c(strata, UPM_boot, n_personas, wt, adj1_wt))
#   
#   # Pesos ajustados por no respuesta, utilizando "strata" como clase de ajuste.
#   boot_output2 <- input_data %>% 
#     group_by(strata) %>%
#     mutate(tot_wt = sum(wt)) %>%
#     filter(I == 1) %>%
#     mutate(adj_nr = tot_wt/sum(wt),
#            adj2_wt = adj_nr*wt) %>%
#     ungroup() %>%
#     select(adj2_wt)
#   
#   # Concatena dataframes.
#   boot_output <- cbind(boot_output1, boot_output2, boot_wt)
#   return(boot_output)
# }


# ## -------------------------------------------- Función para realizar calibración.
# 
# calibracion <- function(df_calib,   # data.frame de muestra con pesos + pesos bootstrap (ajustados y no ajustados por NR)
#                         df_totals,  # data.frame con los totales de las variables de calibracion.
#                         x_vars,     # vector caracter con nombre de variables de calibracion.
#                         n_boot){    # cantidad de muestras bootstrap en df_calib
#   
#   calibv_df  <- as.matrix(df_calib[x_vars])
#   pop_values <- as.vector(unlist(df_totals[x_vars]), mode = 'numeric')
#   col_number <- which(colnames(df_calib) == "wt" )
#   
#   calib_wt <- foreach(i = col_number:ncol(df_calib), .combine=cbind, .packages=c('sampling','foreach')) %dopar% {
#     
#     initial_wt <- as.vector(unlist(df_calib[,i]), mode = 'numeric')
#     g_weights  <- calib(Xs = calibv_df, d = initial_wt, total = pop_values, 
#                         method = "logit", bounds = c(0.4, 3.85))
#     
#     initial_wt*g_weights 
#   }
#   calib_wt <- as.data.frame(calib_wt)
#   
#   # No es lo mas prolijo, pero me devuelve lo que necesito.
#   colnames(calib_wt) <- paste0("cal_", colnames(df_calib[,col_number:ncol(df_calib)]))
#   return(calib_wt)
# } 


