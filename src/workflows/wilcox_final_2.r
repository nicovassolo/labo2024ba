rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

require("data.table")

#------------------------------------------------------------------------------
# 1. Establecer el directorio de trabajo y las rutas de los distintos modelos
#------------------------------------------------------------------------------

# Directorio base donde están los archivos de resultados
setwd("~/buckets/b1/Resultados/") 

# Rutas a los archivos de ganancias para cada modelo
rutas_modelos <- paste0("Estrategia", 5:13, "_tb_ganancias.txt")
rutas_modelos_i <- paste0("Estrategia", 5:13, "i_tb_ganancias.txt")

#------------------------------------------------------------------------------
# 2. Cargar las ganancias de todos los modelos
#------------------------------------------------------------------------------

# Crear una lista para almacenar las tablas de ganancias de cada modelo
ganancias_modelos <- list()

# Crear una variable para almacenar los envíos (asumo que son iguales en todos los modelos)
envios <- NULL

# Cargar las ganancias de cada archivo de cada modelo
for (i in 1:length(rutas_modelos)) {
  tb_modelo <- fread(rutas_modelos[i])
  tb_modelo_i <- fread(rutas_modelos_i[i])
  
  # Unir ambos data.tables y eliminar las 3 primeras columnas
  tb_unido <- cbind(tb_modelo, tb_modelo_i[, -c(1:3), with = FALSE])  # Unir y eliminar columnas
  
  # Guardar la tabla de ganancias unida
  ganancias_modelos[[i]] <- tb_unido
  
  # Guardar la columna de envíos solo una vez (se asume que es la misma para todos)
  if (is.null(envios)) {
    envios <- tb_unido[, envios]
  }
}

#------------------------------------------------------------------------------
# 3. Comparar solo el primer modelo (Estrategia 5) contra el resto
#------------------------------------------------------------------------------

# Crear una tabla para almacenar los resultados de los tests
tb_resultados_wilcoxon <- data.table(
  envio = integer(),
  modelo_1 = character(),
  modelo_2 = character(),
  p_valor = numeric()
)

# Modelo a comparar: Estrategia 5
modelo_1 <- 1  # Índice correspondiente a Estrategia 5
resto_modelos <- setdiff(1:length(ganancias_modelos), modelo_1)

# Comparar Estrategia 5 con el resto
for (k in 1:length(envios)) {  # Iterar sobre cada cantidad de envíos
  for (modelo_2 in resto_modelos) {
    # Extraer las ganancias de los modelos a comparar para el envío 'k'
    modelo_1_ganancias <- unlist(ganancias_modelos[[modelo_1]][k, 4:ncol(ganancias_modelos[[modelo_1]])])  # Columna 4 en adelante
    modelo_2_ganancias <- unlist(ganancias_modelos[[modelo_2]][k, 4:ncol(ganancias_modelos[[modelo_2]])])  # Columna 4 en adelante
    
    # Realizar el test de Wilcoxon para este envío
    test_wilcoxon <- wilcox.test(modelo_1_ganancias, modelo_2_ganancias, paired = TRUE)
    
    # Guardar el resultado en la tabla
    tb_resultados_wilcoxon <- rbind(tb_resultados_wilcoxon, list(
      envio = envios[k],
      modelo_1 = "Estrategia5",  # Nombre del modelo 1
      modelo_2 = paste0("Estrategia", 5 + modelo_2 - 1),  # Ajuste para el nombre de los modelos
      p_valor = test_wilcoxon$p.value
    ))
  }
}

#------------------------------------------------------------------------------
# 4. Aplicar corrección de Bonferroni
#------------------------------------------------------------------------------

# Número total de comparaciones realizadas
num_comparaciones <- nrow(tb_resultados_wilcoxon)

# Aplicar corrección de Bonferroni (alfa / número de comparaciones)
tb_resultados_wilcoxon[, p_valor_ajustado := p.adjust(p_valor, method = "bonferroni")]

#------------------------------------------------------------------------------
# 5. Guardar los resultados
#------------------------------------------------------------------------------

# Guardar los resultados en un archivo .txt
fwrite(tb_resultados_wilcoxon, 
       file = "tb_resultados_wilcoxon_por_envio.txt", 
       sep = "\t")

#------------------------------------------------------------------------------
# 6. Encontrar el p-valor más bajo para cada comparación de modelos
#------------------------------------------------------------------------------

# Crear una tabla para almacenar los p-valores originales y ajustados más bajos y los envíos correspondientes
tb_pvalores_minimos <- tb_resultados_wilcoxon[, .(
  p_valor_minimo = min(p_valor),  # Encontrar el p-valor original más bajo
  envio_p_valor_minimo = envio[which.min(p_valor)],  # Envío donde ocurre el p-valor original más bajo
  p_valor_ajustado_minimo = min(p_valor_ajustado),  # Encontrar el p-valor ajustado más bajo
  envio_p_valor_ajustado_minimo = envio[which.min(p_valor_ajustado)]  # Envío donde ocurre el p-valor ajustado más bajo
), by = modelo_2]

#------------------------------------------------------------------------------
# 7. Guardar los resultados de los p-valores mínimos (original y ajustado)
#------------------------------------------------------------------------------

# Guardar los p-valores mínimos originales y ajustados, junto con las cantidades de envíos correspondientes, en un archivo .txt
fwrite(tb_pvalores_minimos, 
       file = "tb_pvalores_minimos_por_comparacion.txt", 
       sep = "\t")

# Fin del script
