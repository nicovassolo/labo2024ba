#------------------------------------------------------------------------------
# 1. Establecer directorio y archivos
#------------------------------------------------------------------------------

require("data.table")
require("ggplot2")
require("scales")  # Para la función de colores diferenciados
require("ggsci") 

setwd("~/buckets/b1/Resultados/")

# Estrategias y rutas a los archivos
estrategias <- 5:13
rutas_estrategias <- paste0("Estrategia", estrategias, "_tb_ganancias.txt")
rutas_estrategias_i <- paste0("Estrategia", estrategias, "i_tb_ganancias.txt")

#------------------------------------------------------------------------------
# 2. Cargar y unir las ganancias
#------------------------------------------------------------------------------

ganancias_modelos_unidos <- list()

for (i in 1:length(rutas_estrategias)) {
  # Leer las tablas de ambas versiones de cada estrategia
  tb_ganancias <- fread(rutas_estrategias[i])
  tb_ganancias_i <- fread(rutas_estrategias_i[i])
  
  # Unirlas horizontalmente y eliminar las columnas innecesarias
  tb_ganancias_unidas <- cbind(tb_ganancias, tb_ganancias_i[, -c(1:3)])
  
  # Añadir una columna que indique la estrategia correspondiente
  tb_ganancias_unidas[, estrategia := paste0("Estrategia", estrategias[i])]
  
  # Almacenar en la lista
  ganancias_modelos_unidos[[i]] <- tb_ganancias_unidas
}

#------------------------------------------------------------------------------
# 3. Preparar las ganancias promedio para el gráfico
#------------------------------------------------------------------------------

# Unir todas las tablas en una sola
tb_ganancias_unidos <- rbindlist(ganancias_modelos_unidos)

# Extraer solo las columnas de envíos y ganancias (m1 a m20)
ganancias_columnas <- grep("^m", names(tb_ganancias_unidos), value = TRUE)

# Calcular las ganancias promedio para cada combinación de envios y estrategia
tb_ganancias_promedio <- tb_ganancias_unidos[, lapply(.SD, mean), by = .(envios, estrategia), .SDcols = ganancias_columnas]

# Transformar la tabla a formato largo
tb_ganancias_largo <- melt(tb_ganancias_promedio, id.vars = c("envios", "estrategia"), variable.name = "modelo", value.name = "ganancia_promedio")

#------------------------------------------------------------------------------
# 4. Extraer resultados significativos
#------------------------------------------------------------------------------

# Asumimos que ya se ejecutó el código para los tests de Wilcoxon y que `tb_resultados_wilcoxon` está disponible.

# Comparaciones significativas
tb_significativas_sin_bonferroni <- tb_resultados_wilcoxon[modelo_1 == "Estrategia5" & p_valor < 0.05, ]
tb_significativas_con_bonferroni <- tb_resultados_wilcoxon[modelo_1 == "Estrategia5" & p_valor_ajustado < 0.05, ]

# Agregar asteriscos según la significancia
tb_ganancias_largo[, estrategia_etiqueta := estrategia]
tb_ganancias_largo[estrategia %in% tb_significativas_sin_bonferroni$modelo_2, estrategia_etiqueta := paste0(estrategia, "*")]
tb_ganancias_largo[estrategia %in% tb_significativas_con_bonferroni$modelo_2, estrategia_etiqueta := paste0(estrategia, "**")]

#------------------------------------------------------------------------------
# 3. Visualización mejorada: gráfico de líneas con puntos
#------------------------------------------------------------------------------

# Paleta de colores distintiva
colores <- ggsci::scale_color_nejm()  # Colores de la revista NEJM para distinguir mejor

# Crear el gráfico con líneas y puntos
grafico_mejorado <- ggplot(tb_ganancias_largo, aes(x = envios, y = ganancia_promedio, color = estrategia_etiqueta, group = estrategia)) +
  geom_line(size = 1) +  # Líneas
  geom_point(size = 2, position = position_jitter(width = 0.1, height = 0)) +  # Puntos con algo de dispersión
  labs(title = "Ganancias Promedio por Envío para Cada Estrategia",
       subtitle = "Con asteriscos para significancia frente a Estrategia5",
       x = "Cantidad de Envíos",
       y = "Ganancia Promedio",
       color = "Estrategia") +
  scale_y_continuous(labels = scales::comma) +  # Formatear valores en el eje Y
  colores +  # Aplicar la paleta de colores
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10),  # Hacer la leyenda más legible
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje X para mejor visualización
        axis.text = element_text(size = 12),  # Hacer el texto de los ejes más grande
        plot.title = element_text(size = 16, face = "bold"),  # Título destacado
        plot.subtitle = element_text(size = 12))  # Subtítulo más pequeño

# Mostrar el gráfico mejorado
print(grafico_mejorado)
