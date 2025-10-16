---
  title: "Informe técnico del Lago Cocibolca.Rmd"
# author: "Felipe Mendoza Arriaza"
output: 
  rmarkdown::github_document:
  fig_width: 6
fig_height: 4
---
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

``` 
<!-- # Pre-requisitos -->
  ```{r, include=FALSE} 
packages <- c("sf", "spdep", "spatialreg", "vioplot", "corrplot", "ggplot2")
# Revisar si los paquetes están instalados
no_i<- packages[!packages %in% installed.packages()[, "Package"]]
# instalar paquetes faltantes
if (length(no_i) > 1) {
  install.packages(no_i, repos = "https://cran.rstudio.com/", dep = T)
}

library(spdep)   # datos espaciales
library(sf)      # datos espaciales
library(spatialreg) # modelos SAR/SEM
library(vioplot)   # gráficos de violín
library(corrplot)  # gráficos de correlación 
library(ggplot2)

unlink("C:/Users/DELL/AppData/Local/R/win-library/4.3/00LOCK-ggplot2", recursive = TRUE)
install.packages("ggplot2")

```
library(readxl)

dm <- read_excel("C:/Users/DELL/Documents/2025/Curso R/Datos_Cocibolca_2.xlsx")

head(dm)   # primeras 6 filas
summary(dm)  # resumen estadístico

#  Vistazo general de las variables
# Función para graficar histograma y boxplot en una misma ventana




# Ajustar la ventana gráfica: 1 fila y 2 columnas
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))  # mar ajusta los márgenes

# Histograma de pH
hist(dm$pH,
     main = "Histograma de pH",
     xlab = "pH",
     col = "lightblue",
     breaks = 10)

# Histograma de Temperatura
hist(dm$Temperatura,
     main = "Histograma de Temperatura",
     xlab = "Temperatura (°C)",
     col = "salmon",
     breaks = 10)

par(mfrow = c(1,1))

num_cols <- sapply(dm[,3:5], is.numeric)

lapply(dm[,3:5][, num_cols], hist)
lapply(dm[,3:5][, num_cols], boxplot)

# Seleccionar las columnas de interés
cols <- c("Redox", "OD", "Sat. O2")

# Ajustar la ventana gráfica: 1 fila, 3 columnas
par(mfrow=c(1,length(cols)), mar=c(4,4,2,1))

# Graficar violin plots
for (col in cols) {
  vioplot(dm[[col]],
          main = col,
          col = "lightblue",
          horizontal = TRUE)
}

# Restaurar la ventana a 1 gráfico
par(mfrow=c(1,1))

# Seleccionar columnas
cols <- c("Redox", "OD", "Sat. O2")

# Matriz de correlaciones
(cor_mat <- cor(dm[, cols], use="complete.obs"))  # use="complete.obs" ignora NAs

# Scatterplot matrix
pairs(dm[, cols], pch=19, col="lightblue", main="Gráficos de dispersión")


cols <- c("Redox", "OD", "Sat. O2")

# Matriz de correlación
cor_mat <- cor(dm[, cols], use="complete.obs")

# Corrplot
corrplot(cor_mat, method = "circle", type = "upper",
         col = colorRampPalette(c("orangered", "white", "mediumseagreen"))(200),
         tl.col = "darkslateblue", tl.srt = 45, 
         addCoef.col = "midnightblue", number.cex = 0.8)


# Convertir a objeto espacial
dm_sp <- st_as_sf(dm, coords = c("Este_UTM", "Norte_UTM"), crs = 32616)

dm_sp <- st_transform(dm_sp, crs = 4326)
                      
plot(dm_sp["Temperatura"],
cex = scales::rescale(dm_sp$Temperatura, to = c(1,4)),
col = "tomato", pch = 21,
main = "Temperatura (°C)", axes = TRUE)                    


ggplot(dm_sp) +
  geom_sf(aes(color = Temperatura, size = Temperatura)) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  ggtitle("Mapa de Temperatura") +
  guides(size = guide_legend(title="Temp (°C)"), color = guide_colorbar(title="Temp (°C)"))


library(sf)
dm_sf <- st_as_sf(dm, coords = c("Este_UTM", "Norte_UTM"), crs = 32616)
dm_sf <- st_transform(dm_sf, crs = 4326)  # ahora en lon/lat
dm <- cbind(dm, st_coordinates(dm_sf))    # agrega columnas X (lon) y Y (lat)
colnames(dm)[(ncol(dm)-1):ncol(dm)] <- c("lon", "lat")


# Definir límites del mapa según tus datos
xlim <- range(dm$lon) + c(-0.05, 0.05)
ylim <- range(dm$lat) + c(-0.05, 0.05)


dm <- dm[, !duplicated(names(dm))]

ggplot() +
  borders("world", xlim = xlim, ylim = ylim,
          fill = "gray90", colour = "gray70") +
  geom_point(data = dm,
             aes(x = lon, y = lat, colour = Temperatura),
             size = 2) +
  scale_color_gradient(low = "lightblue", high = "darkred") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "Mapa de Temperatura",
       x = "Longitud", y = "Latitud", colour = "Temp (°C)") +
  theme_minimal()


# Mapa por cuartiles de temperatura
ggplot(dm, aes(x = lon, y = lat)) +
  borders("world", xlim = range(dm$lon), ylim = range(dm$lat),
          fill = "gray90", colour = "gray70") +
  geom_point(aes(color = cut_number(Temperatura, n=4)), size = 2 ) +
  scale_color_brewer(palette = "RdYlBu", name = "Cuartiles Temp") +
  coord_sf(xlim = range(dm$lon), ylim = range(dm$lat), expand = FALSE) +
  theme_minimal() +
  labs(title = "Temperatura (°C)")

# Cuartiles de temperatura
quantile(dm$Temperatura, probs = seq(0, 1, 0.25))


ggplot(dm, aes(x = lon, y = lat)) +
  borders("world", fill="gray90", colour="gray70") +
  geom_point(aes(size = OD, color = `Sat. O2`), alpha = 0.6) +
  scale_color_viridis_c() +
  coord_sf(xlim = range(dm$lon), ylim = range(dm$lat)) +
  theme_minimal() +
  labs(size = "OD", color = "Sat. O2")


# Supongamos que ya tienes lon/lat en tu df dm
# Mapear Total Iron usando tamaño y color por cuartiles
ggplot(dm, aes(x = lon, y = lat)) +
  borders("world", fill="gray90", colour="gray70") +
  geom_point(aes(size = `pH`, color = cut_number(`pH`, 4)), alpha = 0.7) +
  scale_color_brewer(palette = "RdYlBu", name = "Cuartiles pH") +
  coord_sf(xlim = range(dm$lon), ylim = range(dm$lat)) +
  theme_minimal() +
  labs(size = "pH",
       title = "Mapa de pH por cuartiles")


# Calculo de cuartiles de pH
quantile(dm$`pH`, probs = seq(0, 1, 0.25))

# Numéricamente
# Para ver los descriptivos de la variable Temperatura según el valor de la variable pH:
aggregate(Temperatura ~ cut_number(pH, 4), data = dm, FUN = summary)


vb <- dm$`Redox`  # reemplazá con la variable que quieras analizar

q <- quantile(vb, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)  # importante na.rm=TRUE
iqr <- IQR(vb, na.rm = TRUE)

lw <- q[1] - 1.5 * iqr
up <- q[3] + 1.5 * iqr

dm$boxcat <- cut(
  vb,
  breaks = c(-Inf, lw, q[1], q[2], q[3], up, Inf),
  labels = c("Extremo bajo", "Q1", "Q2", "Q3", "Q4", "Extremo alto"),
  include.lowest = TRUE)
  
  xlim <- range(dm$lon, na.rm = TRUE) + c(-0.2, 0.2)
  ylim <- range(dm$lat, na.rm = TRUE) + c(-0.2, 0.2)
  
  ggplot(dm, aes(x = lon, y = lat)) +
    borders("world", xlim = xlim, ylim = ylim,
            fill = "gray90", colour = "gray70") +
    geom_point(aes(color = boxcat), size = 3) +
    scale_color_manual(values = c(
      "Extremo bajo" = "blue",
      "Q1"           = "lightblue",
      "Q2"           = "green",
      "Q3"           = "orange",
      "Q4"           = "red",
      "Extremo alto" = "darkred"
    )) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(title = paste("Distribución espacial de", deparse(substitute(vb))),
         x = "Longitud", y = "Latitud",
         color = "Categoría")
  
  
  # Cargar librería
  library(spdep)
  
  # Convertir coordenadas a numérico
  coords <- cbind(as.numeric(dm$lon), as.numeric(dm$lat))
  
  # --- Vecinos por distancia ---
  # Elegir un radio en metros (ejemplo: 5000 m = 5 km)
  range_dist <- 5000  
  nb_dist <- dnearneigh(coords, 0, range_dist)
  
  # --- Vecinos k-NN ---
  k <- 4
  nb_knn <- knn2nb(knearneigh(coords, k = k))
  
  # --- Graficar ---
  par(mfrow = c(1,2))
  plot(nb_dist, coords, main = paste("Vecinos por distancia (0-", range_dist, " m)", sep=""), col = "deepskyblue")
  plot(nb_knn, coords, main = paste("Vecinos k-NN (k=", k, ")", sep=""), col = "coral")
  par(mfrow=c(1,1))  # Restaurar configuración
)




# Ahora si podemos ver los outliers espacialmente.
```{r espacial8, echo=T}

W <- nb2listw(nb_knn, style = "W")

# Moran's I
lisa <- localmoran(dm$Redox, W)

# Creando las etiquetas tomando como referencia a la media
dm$lisa_cat <- NA
ref_val <- mean(dm$Redox)

for (i in 1:nrow(dm)) {
  if (dm$Redox[i] >= ref_val & lisa[i,1] > 0) {
    dm$lisa_cat[i] <- "High-High"
  } else if (dm$Redox[i] < ref_val & lisa[i,1] > 0) {
    dm$lisa_cat[i] <- "Low-Low"
  } else if (dm$Redox[i] >= ref_val & lisa[i,1] < 0) {
    dm$lisa_cat[i] <- "High-Low"
  } else if (dm$Redox[i] < ref_val & lisa[i,1] < 0) {
    dm$lisa_cat[i] <- "Low-High"
  }
}

dm$lisa_cat <- factor(dm$lisa_cat,
                      levels = c("High-High","Low-Low","High-Low","Low-High"))

# El gráfico
xlim <- range(dm$lon) + c(-0.2, 0.2)
ylim <- range(dm$lat) + c(-0.2, 0.2)

ggplot(dm, aes(x = lon, y = lat)) +
  borders("world", xlim = xlim, ylim = ylim,
          fill = "gray90", colour = "gray70") +
  geom_point(aes(color = lisa_cat), size = 3) +
  scale_color_manual(values = c(
    "High-High" = "darkred",
    "Low-Low"   = "darkblue",
    "High-Low"  = "orange",
    "Low-High"  = "skyblue"
  )) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_minimal() +
  labs(title = "Mapa de outliers espaciales LISA (Redox)",
       x = "Longitud", y = "Latitud",
       color = "Categoría LISA")