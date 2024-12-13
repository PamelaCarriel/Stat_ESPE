############### Generar un dataset en el contexto de TI ########################
#Variables:
#Sistema Operativo (cualitativa): Linux, Windows, macOS.
#Tipo de Servidor (cualitativa): Base de datos, Web, Aplicaciones.
#Tiempo de Respuesta (cuantitativa): tiempo promedio en milisegundos (ms).
#Carga de CPU (cuantitativa): porcentaje promedio de uso de CPU (%).

set.seed(123) # Para reproducibilidad
data <- data.frame(
  Sistema = sample(c("Linux", "Windows", "macOS"), 100, replace = TRUE),
  Servidor = sample(c("Base de datos", "Web", "Aplicaciones"), 100, replace = TRUE),
  TiempoRespuesta = rnorm(100, mean = 200, sd = 50), # Cuantitativa (ms)
  CargaCPU = runif(100, min = 20, max = 90) # Cuantitativa (%)
)
head(data)  # Muestra las primeras 6 filas
tail(data)  # Muestra las últimas 6 filas
View(data)  # Abre una vista interactiva 

#-----------------------------------------------------------------------------#
####################### Representación Tabular  ###############################
# Tablas de frecuencia para observar la distribución en variable Cualitativas.

table(data$Sistema)
table(data$Servidor)

# Resumen estadístico con medidas como la media, mediana, y rango.en variable 
# Cuantitativas
summary(data$TiempoRespuesta)
summary(data$CargaCPU)

#-----------------------------------------------------------------------------#
####################### Representación Gráfica  ###############################
library(ggplot2)
library(patchwork)
# Gráficos de barras para mostrar frecuencias en variables Cualitativas
ggplot(data, aes(x = Sistema)) + geom_bar() + ggtitle("Distribución por Sistema Operativo")
ggplot(data, aes(x = Servidor)) + geom_bar() + ggtitle("Distribución por Tipo de Servidor")

# Histogramas y boxplots histogramas para ver la distribución y boxplots para 
# analizar la dispersión en variables Cuantitativas
(hist_tiempo + box_tiempo) / (hist_carga + box_carga)
par(mfrow = c(2, 2))  # Configura una cuadrícula de 2x2
hist(data$TiempoRespuesta, main = "Histograma de Tiempo de Respuesta", xlab = "Tiempo (ms)")
boxplot(data$TiempoRespuesta, main = "Boxplot de Tiempo de Respuesta")
hist(data$CargaCPU, main = "Histograma de Carga de CPU", xlab = "Carga (%)")
boxplot(data$CargaCPU, main = "Boxplot de Carga de CPU")

#-----------------------------------------------------------------------------#
####################### Características Numéricas   ############################

# Desviación estándar podemos ver la Homogeniedad 
sd(data$TiempoRespuesta)           # 51.11892
sd(data$CargaCPU)                  # 20.58926

# Prueba de normalidad podemos ver la normallidad
shapiro.test(data$TiempoRespuesta) #p-value = 0.04013
shapiro.test(data$CargaCPU)        #p-value = 0.0005709

########--------Interpretación de resultados------------#######
#¿LOS DATOS SON HOMOGÉNEOS?
###Si la desviación estándar es baja en comparación con la media, los datos son homogéneos###
# En nuestro caso tanto el Tiempo de respuesta es homogeneo, puesto que la D. Estandar es de 
# 51.11892 es menor que la media de la misma variable que es de 199.62 
# Al igual que la variable CargaCPU es homogeneo, ya que la D. Estandar es de 20.58926
# es menor que la media que es 54.28

# ¿SOSPECHA DE NORMALIDAD?
###Si el p-valor de la prueba Shapiro-Wilk es mayor a 0.05, los datos podrían ajustarse a una distribución normal.###
# En nuestra variable TiempoRespuesta tenemeos que nuestro p valor es de 0.04013 es menor a 0.05, 
#lo que sugiere que los datos no siguen una distribución normal.
# En la cariable CargaCPU tenemos que nuestro p valor es de 0.0005709 lo que es mucho menor a 0.05,
# indicando que los datos de Carga de CPU sugieren que no son normales.
