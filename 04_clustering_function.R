# Ultima actualizacion: 30/1/2019.
# Autor: Tomas Capretto.

#-------------------------------------------------- Descripción -------------------------------------------------- #
# Este programa crea la funcion necesaria para construir areas de muestreo de aproximadamente 200 viviendas        #
# dentro de cada localidad del Estado de Texas.                                                                    #
# La funcion llamada Iterative nearest neighbor es tomada de:                                                      #
# https://github.com/jmonlong/Hippocamplus/blob/master/content/post/2018-06-09-ClusterEqualSize.Rmd                # 
# Descripcion:                                                                                                     #
# Se toma a un punto y a sus vecinos mas cercanos y se los asigna a un cluster.  Los puntos son removidos          #
# del listado a medida que se les asigna grupo.                                                                    #
#                                                                                                                  #                                                                                                      #
# Pasos:                                                                                                           #
#   Mientras haya mas de 'S' punto sin asignar                                                                     #
# - Se selecciona un punto, aleatoriamente o siguiendo una regla particular (ver debajo).                          #
# - Se calcula distancia a cada uno de los puntos. El punto  seleccionado y los S-1 vecinos mas cercanos son       #
#   asignados a un nuevo cluster.                                                                                  #
# - Estos puntos son removidos del listado, evitando entrar en un bucle y/o asignar puntos a multiples grupos.     #   
# - Si el numero total de puntos no es multiplo de S, los puntos restantes son asignados a su propio cluster       #
#   o a un cluster existente.                                                                                      #
#                                                                                                                  #   
# Sobre el primer paso:                                                                                            #
# - El primer punto elegido es seleccionado al azar. Luego de formar el primer cluster, el segundo punto,          #
#   y los sucesivos puntos, pueden ser elegidos de la siguiente forma:                                             #  
#   - Aleatorio: El siguiente punto es elegido al azar.                                                            #
#   - Distancia minima: El siguiente punto es el que se encuentra mas cercano a los puntos que fueron agrupados.   #
#   - Distancia maxima: El siguiente punto es el que se encuentra mas lejano a los puntos que fueron agrupados.    #
#----------------------------------------------------------------------------------------------------------------- #

nnit <- function(mat, clsize = 10, method=c('random','maxd', 'mind')){
  clsize.rle = rle(as.numeric(cut(1:nrow(mat), ceiling(nrow(mat)/clsize))))
  clsize = clsize.rle$lengths
  lab = rep(NA, nrow(mat))
  dmat = as.matrix(dist(mat))
  cpt = 1
  while(sum(is.na(lab)) > 0){
    lab.ii = which(is.na(lab))
    dmat.m = dmat[lab.ii,lab.ii]
    if(method[1]=='random'){
      ii = sample.int(nrow(dmat.m),1)
    } else if(method[1]=='maxd'){
      ii = which.max(rowSums(dmat.m))
    } else if(method[1]=='mind'){
      ii = which.min(rowSums(dmat.m))
    } else {
      stop('unknown method')
    }
    lab.m = rep(NA, length(lab.ii))
    lab.m[head(order(dmat.m[ii,]), clsize[cpt])] = cpt
    lab[lab.ii] = lab.m
    cpt = cpt + 1
  }
  if(any(is.na(lab))){
    lab[which(is.na(lab))] = cpt
  }
  lab
}

