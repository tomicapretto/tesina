PATH1 = "C:/Users/Tomi/Google Drive/tesina"
setwd(PATH1) ; rm(PATH1)

# Carga de librerias.
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(purrr)

# Desactivo notación científica.
# scipen = integer. 
# A penalty to be applied when deciding to print numeric values in fixed or exponential notation. 
# Positive values bias towards fixed and negative towards scientific notation: 
#   fixed notation will be preferred unless it is more than scipen digits wider.

options(scipen = 999)

# ---------------------------------------- Manipulación de datos.
hh_data <- fread("./datos/bases/hh_data_5.csv")
pop_mean <- mean(hh_data$hh_income) ; rm(hh_data)
col_names <- c("no_adj", "adj_nr1", "adj_nr2", "cal", "adj_nr1_cal", "adj_nr2_cal")

# Importación de resultados de simulación
result_files <- list.files(path = "./datos/sim_output/", full.names = TRUE, pattern = '_nfpc')

results <- lapply(result_files, read.csv)
results <- lapply(results, function(x) {
                            class(x) <- "data.frame"
                            colnames(x) <- col_names
                            x})

resultado_media <- list(
  "MCAR" = list("T60" = results[[3]], "T80" = results[[9]]),
  "MAR"  = list("T60" = results[[1]], "T80" = results[[7]]),
  "NMAR" = list("T60" = results[[5]], "T80" = results[[11]])
)

resultado_var <- list(
  "MCAR" = list("T60" = results[[4]], "T80" = results[[10]]),
  "MAR"  = list("T60" = results[[2]], "T80" = results[[8]]),
  "NMAR" = list("T60" = results[[6]], "T80" = results[[12]])
)

rm(results, col_names, result_files)

# ---------------------------------------- Calculo de medidas de interes.
exp_mean = rb_mean = sim_var = ecm_mean = exp_var = rb_var = var_var = list()

for (i in c("MCAR", "MAR", "NMAR")){
  for (j in c("T60", "T80")){
    
    # Media esperada: Media de las estimaciones puntuales.
    exp_mean[[i]][[j]] <- apply(resultado_media[[i]][[j]], 2, mean) 
    
    # Sesgo relativo de la media.
    rb_mean[[i]][[j]]  <- 100*(exp_mean[[i]][[j]] - pop_mean)/pop_mean  
    
    # Variancia simulada: Variancia de las estimaciones puntuales.
    sim_var[[i]][[j]]  <- apply(resultado_media[[i]][[j]], 2, var)
    
    # Error cuadrático medio de la media: Variancia simulada + Sesgo^2
    ecm_mean[[i]][[j]] <- sim_var[[i]][[j]] + (exp_mean[[i]][[j]] - pop_mean)^2
    
    # Variancia esperada: Media de las variancias estimadas.
    exp_var[[i]][[j]]  <- apply(resultado_var[[i]][[j]], 2, mean)
    
    # Sesgo relativo de la variancia.
    rb_var[[i]][[j]]   <- 100*(exp_var[[i]][[j]] - sim_var[[i]][[j]])/sim_var[[i]][[j]] 
    
    # Variancia del estimador de variancia
    var_var[[i]][[j]]  <- apply(resultado_var[[i]][[j]], 2, var) 
  }
}


# ---------------------------------------- Boxplots para la distribucion del estimador de variancia.
# Etiquetas para los graficos
x_labels = c("Sin ajuste", "Ajuste NR1", "Ajuste NR2", "Calibracion", "NR1 + Calib", "NR2 + Calib")

plots <- list()
for (i in c("MCAR", "MAR", "NMAR")){
  for (j in c("T60", "T80")){

    df <- resultado_var[[i]][[j]]
    
    if (j == "T60"){
      plot_title <- paste0(i, " - Tasa de respuesta 60%")
    } else {
      plot_title <- paste0(i, " - Tasa de respuesta 80%")
    }

    if (i == "NMAR") {y_height = 500000} else {y_height = 1500000}
    
    plot <-
    ggplot(stack(df), aes(x = factor(ind, levels = names(df)), y = values)) + 
      geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
      geom_point(data = stack(sim_var[[i]][[j]]), 
                 aes(x = factor(ind, levels = names(sim_var[[i]][[j]])), y = values, colour = "Variancia simulada"),
                 shape = 17, size = 3, show.legend = T) +
      geom_point(data = stack(exp_var[[i]][[j]]), 
                 aes(x = factor(ind, levels = names(exp_var[[i]][[j]])), y = values, colour = "Variancia esperada"),
                 shape = 16, size = 3, show.legend = T) +
      geom_text(data = stack(rb_var[[i]][[j]]), 
                aes(x = factor(ind, levels = names(rb_var[[i]][[j]])), y = y_height, label = paste0(round(values,2),"%")), 
                col='black', size = 3) + 
      scale_y_continuous(name = "Variancia estimada",
                         breaks = seq(500000, 7000000, 1000000),
                         limits = c(200000, 7000000)) + 
      scale_x_discrete(name = "Ajuste realizado",
                       labels = x_labels) + 
      ggtitle(plot_title) +
      labs(caption = "El valor bajo cada boxplot es el sesgo relativo del estimador") +
      scale_colour_manual("",
                          breaks = c("Variancia esperada", "Variancia simulada"),
                          values = c("#000080", "#B22222")) +
      guides(shape = FALSE,
             colour = guide_legend(override.aes = list(shape = c(16, 17))))+
      theme_bw() + 
      theme(
        legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)
      ) 
    
    plots[[length(plots)+1]] <- plot
  }
}  

names <- paste0("var_", paste0(c(rep("MCAR", 2), rep("MAR", 2), rep("NMAR", 2)), c("_T60", "_T80")))

paths <- paste0("imgs/",names,".png") 
pwalk(list(filename = paths, plot = plots), ggsave, width = 13, height = 9.5, units = "cm")

# ---------------------------------------- Boxplots para la distribucion del estimador de la media.

plots <- list()
for (i in c("MCAR", "MAR", "NMAR")){
  for (j in c("T60", "T80")){

    df <- resultado_media[[i]][[j]]
  
    if (j == "T60"){
      plot_title <- paste0(i, " - Tasa de respuesta 60%")
    } else {
      plot_title <- paste0(i, " - Tasa de respuesta 80%")
    }

   plot <-
      ggplot(stack(df), aes(x = factor(ind, levels = names(df)), y = values)) + 
      geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
      geom_hline(yintercept = pop_mean, size = 1.2, color = "#B22222") +
      geom_text(aes(x = 5.2, y = pop_mean, label = "Ingreso medio poblacional"), 
                colour = "#B22222", vjust = 1.1,
                data = data.frame()) +
      geom_point(data = stack(exp_mean[[i]][[j]]), 
                 aes(x = factor(ind, levels = names(exp_var[[i]][[j]])), y = values),
                 color = "#000080", size = 3) +
      geom_text(data = stack(rb_mean[[i]][[j]]), 
                aes(x = factor(ind, levels = names(rb_mean[[i]][[j]])), y = 62500, label = paste0(round(values,3),"%")), 
                col='black', size = 3) + 
      scale_y_continuous(name = "Ingreso medio estimado") +
      scale_x_discrete(name = "Ajuste realizado",
                       labels = x_labels) + 
      ggtitle(plot_title) +
      labs(caption = "El valor bajo cada boxplot es el sesgo relativo del estimador") +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5))
    
    plots[[length(plots)+1]] <- plot
  }
}  
names <- paste0("mean_", paste0(c(rep("MCAR", 2), rep("MAR", 2), rep("NMAR", 2)), c("_T60", "_T80")))

paths <- paste0("imgs/",names,".png") 
pwalk(list(filename = paths, plot = plots), ggsave, width = 13, height = 9.5, units = "cm")

# ---------------------------------------- Creacion de data frames
df_summary <- data.frame(
  c(rep("MCAR", 12), rep("MAR", 12), rep("NMAR", 12)),
  rep(c(rep("60%", 6), rep("80%", 6)),3),
  factor(rep(x_labels, 6), levels = x_labels),
  unname(unlist(exp_mean)),
  unname(unlist(rb_mean)),
  unname(unlist(sim_var)),
  unname(unlist(ecm_mean)),
  unname(unlist(exp_var)),
  unname(unlist(rb_var)),
  unname(unlist(var_var))
)

colnames(df_summary) <- c("mecanica", "tasa", "estimador", "exp_mean", "rb_mean", "sim_var", 
                          "ecm_mean", "exp_var", "rb_var","var_var")


df_summary %>% filter(tasa == "80%") %>% select(rb_mean, sim_var, ecm_mean, rb_var, var_var)
df_summary %>% filter(tasa == "60%") %>% select(rb_mean, sim_var, ecm_mean, rb_var, var_var)


# ---------------------------------------- Lineplots.
plots <- list()
for (i in c("MCAR", "MAR", "NMAR")){
  df_plot <- df_summary %>% filter(mecanica == i)
  
  for (ms in c("rb_mean", "rb_var", "exp_var", "sim_var", "var_var", "ecm_mean")){

    ylab <- switch(ms,
                   rb_mean = "Sesgo relativo (%)",
                   rb_var  = "Sesgo relativo (%)",
                   exp_var = "Variancia esperada",
                   sim_var = "Variancia simulada",
                   var_var = "Variancia del estimador de variancia",
                   ecm_mean = "Error cuadrático medio")
    
    title <- switch(ms,
                    rb_mean = "Sesgo relativo del estimador del ingreso medio",
                    rb_var  = "Sesgo relativo del estimador de variancia",
                    exp_var = "Variancia esperada del estimador del ingreso medio",
                    sim_var = "Variancia simulada del estimador del ingreso medio",
                    var_var = "Variancia del estimador de variancia",
                    ecm_mean = "Error cuadrático medio del estimador del ingreso medio")
    
     plot <-
       ggplot(df_plot, aes_string(x = "estimador", y = ms, group = "tasa")) + 
       geom_line(aes(linetype = tasa, color = tasa), lwd = 1.2) +
       geom_point(aes(color = tasa), lwd = 2) + 
       labs(title = title,
            x = "Ajuste realizado", y = ylab,
            linetype = "Tasa de respuesta") +
       scale_color_manual(values = c("#DA0A0A", "#4271AE")) +
       guides(colour = FALSE,
              linetype = guide_legend(override.aes = list(color = c("#DA0A0A", "#4271AE"))))+
       theme_bw() + 
       theme(
         legend.direction = "horizontal", 
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5)
       )
     
     plots[[length(plots)+1]] <- plot
     
  }
}

names <- do.call(paste0, 
                 expand.grid(c("rb_mean", "rb_var", "exp_var", "sim_var", "var_var", "ecm_mean"), c("_MCAR", "_MAR", "_NMAR")))

paths <- paste0("imgs/",names,".png") 
pwalk(list(filename = paths, plot = plots), ggsave, width = 13.5, height = 9.5, units = "cm")
