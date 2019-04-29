# Ultima actualizacion: 28/04/2019.
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa contiene las funciones necesarias para realizar las simulaciones.                                  #
# También incluye los coeficientes para simular perdidos en MAR.                                                   #
# En la medida de lo posible, se buscó dar una descripción del funcionamiento de las funciones y sus argumentos.   #
#                                                                                                                  #
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
    # Para cada conjunto de pesos de replicación calcula el estimador de Hajek en cada vector de pesos.
    # Luego calcula la variancia bootstrap utilizando estas medias.
    
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