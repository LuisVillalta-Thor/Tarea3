library(dplyr)
library(ggplot2)

#Analisis de datos continuos
datoscont <- datos %>%
  select(DejaBanco,ScoringCrediticio,Edad,BalanceCuenta,CantidadProductos,SalarioEstimado)

nombres <- as.data.frame(colnames(datoscont))
nombrest <- t(nombres)

Analisiscont<-function(x) {
  nombres <- as.data.frame(colnames(datoscont))
  nombrest <- t(nombres)

  # VARIABLE
  p1 <- ggplot(data = datos, aes(x = datoscont[,i], fill = DejaBanco)) +
    geom_density(alpha = 0.5) +
    labs(title=paste("Densidad variable",nombrest[,i]),x=nombrest[,i], y = "Densidad") +
    scale_fill_manual(values = c("gray50", "orangered2")) +
    geom_rug(aes(color = DejaBanco), alpha = 0.5) +
    scale_color_manual(values = c("gray50", "orangered2")) +
    theme_bw()
  p2 <- ggplot(data = datos, aes(x = DejaBanco, y = datoscont[,i], color = DejaBanco)) +
    geom_boxplot(outlier.shape = NA) +
    labs(title=paste("Boxplot Variable",nombrest[,i]),x=nombrest[,i], y = "Boxplot") +
    geom_jitter(alpha = 0.3, width = 0.15) +
    scale_color_manual(values = c("gray50", "orangered2")) +
    theme_bw()


  # Estadísticos del saldo operación de los asociados
  # Tiene que guardar la variable como un objeto porque si no se muestra
  x <- datoscont %>% filter(!is.na(datoscont[,i])) %>% group_by(DejaBanco) %>%
    summarise(media = mean(datoscont[,i]),
              mediana = median(datoscont[,i]),
              min = min(datoscont[,i]),
              max = max(datoscont[,i]))
  print(x)
  # Busca outliers en la variable saldo operación
  boxplot(datoscont[,i])
  plot(p1)
  plot(p2)
}

#For para corrida de las variables
inicio <- 6
fin <- dim(datoscont)[2]

for(i in inicio:fin)
{
  Analisiscont(datoscont[,i])
}

#Analisis de datos categoricos
datoscat <- datos %>%
  select(DejaBanco,Pais,Genero,TarjetaCredito,Activo)

Analisiscat<-function(x) {

  nombres <- as.data.frame(colnames(datoscat))
  nombrest <- t(nombres)
  g <- ggplot(data = datoscat, aes(x = datoscat[,i], y = ..count.., fill = DejaBanco)) +
    geom_bar() +
    labs(title=paste("Gráfico de barras variable",nombrest[,i]),x=nombrest[,i], y = "Cantidad") +
    scale_fill_manual(values = c("gray50", "orangered2")) +
    theme_bw() +
    theme(legend.position = "bottom")

  # Tabla de frecuencias relativas para la variable
  p <- prop.table(table(datoscat[,i], datos$DejaBanco), margin = 1) %>% round(digits = 2)
  plot(g)
  print(p)
}

#For para corrida de las variables
inicio <- 2
fin <- dim(datoscat)[2]

for(i in inicio:fin)
{
  Analisiscat(datoscat[,i])
}

