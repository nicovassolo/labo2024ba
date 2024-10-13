rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

require("data.table")

#------------------------------------------------------------------------------
# Configuración del entorno de trabajo
#------------------------------------------------------------------------------

# Establezco el directorio de trabajo
setwd("~/buckets/b1/") 

# Rutas de los resultados de los dos modelos
ruta_modelo_1 <- "~/buckets/b1/flow/wf_septiembre-024/011-KA_evaluate_kaggle/tb_ganancias.txt"
ruta_modelo_2 <- "~/buckets/b1/flow/wf_septiembre-028/011-KA_evaluate_kaggle/tb_ganancias.txt"

# Cargar los datos de los dos modelos
tb_modelo_1 <- fread(ruta_modelo_1)
tb_modelo_2 <- fread(ruta_modelo_2)

#------------------------------------------------------------------------------
# Preparar las tablas para la comparación
#------------------------------------------------------------------------------

# Seleccionar las columnas de las ganancias para cada modelo
# Se asume que las primeras columnas son 'envios' y 'rank', y las siguientes 20 son las ganancias
ganancias_modelo_1 <- tb_modelo_1[, .SD, .SDcols = 3:22]  # Columnas con las 20 semillas del modelo 1
ganancias_modelo_2 <- tb_modelo_2[, .SD, .SDcols = 3:22]  # Columnas con las 20 semillas del modelo 2

# Extraer la columna de envíos (asumo que la primera columna es 'envios')
envios <- tb_modelo_1[, envios]

#------------------------------------------------------------------------------
# Realizar la prueba de Wilcoxon para cada vector de envíos
#------------------------------------------------------------------------------

# Crear tabla para almacenar los resultados de la comparación
tb_comparacion_wilcoxon <- data.table(
  envios = integer(),
  pvalue = numeric(),   # El p-value de la prueba de Wilcoxon
  ganancia_media_modelo_1 = numeric(),  # Promedio de ganancia del modelo 1
  ganancia_media_modelo_2 = numeric()   # Promedio de ganancia del modelo 2
)

# Realizo la prueba de Wilcoxon a dos colas para cada cantidad de envíos
for (i in 1:nrow(ganancias_modelo_1)) {
  # Ganancias de las 20 semillas para la fila actual (cantidad de envíos)
  ganancias_1 <- unlist(ganancias_modelo_1[i,])
  ganancias_2 <- unlist(ganancias_modelo_2[i,])
  
  # Prueba de Wilcoxon entre las 20 semillas de ambos modelos para la misma cantidad de envíos
  wt <- wilcox.test(
    ganancias_1,  # Vector con las 20 semillas del modelo 1
    ganancias_2,  # Vector con las 20 semillas del modelo 2
    paired = TRUE  # Los modelos se comparan en pares (misma semilla)
  )
  
  # Agregar los resultados a la tabla de comparación
  tb_comparacion_wilcoxon <- rbind(tb_comparacion_wilcoxon, list(
    envios = envios[i],
    pvalue = wt$p.value,  # Valor p de la prueba de Wilcoxon
    ganancia_media_modelo_1 = mean(ganancias_1),
    ganancia_media_modelo_2 = mean(ganancias_2)
  ))
}

#------------------------------------------------------------------------------
# Guardar los resultados en un archivo
#------------------------------------------------------------------------------

# Guardar los resultados de la comparación en un archivo .txt
fwrite(tb_comparacion_wilcoxon, 
       file = "tb_comparacion_wilcoxon_5vs7.txt", 
       sep = "\t")

# Fin del script
