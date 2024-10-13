rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

require("data.table")

#------------------------------------------------------------------------------
# 1. Establecer el directorio de trabajo y las rutas de los distintos modelos
#------------------------------------------------------------------------------

# Directorio base donde están los distintos workflows
setwd("~/buckets/b1/flow/") 

# Listado de carpetas de los distintos modelos (workflows)
workflows <- c("wf_septiembre-024", "wf_septiembre-025", "wf_septiembre-028", "wf_septiembre-029", 
               "wf_septiembre-030", "wf_septiembre-031", "wf_septiembre-032", "wf_septiembre-033",
               "wf_septiembre-034", "wf_septiembre-035", "wf_septiembre-036", "wf_septiembre-037")

# Rutas a los archivos de ganancias para cada modelo
rutas_modelos <- paste0("~/buckets/b1/flow/", workflows, "/011-KA_evaluate_kaggle/tb_ganancias.txt")

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
  
  # Seleccionar las columnas de ganancias (columnas 3 a 22)
  ganancias_modelos[[i]] <- tb_modelo[, .SD, .SDcols = 3:22]
  
  # Guardar la columna de envíos solo una vez (se asume que es la misma para todos)
  if (is.null(envios)) {
    envios <- tb_modelo[, envios]
  }
}

#------------------------------------------------------------------------------
# 3. Comparar las ganancias por cada cantidad de envíos con el test de Wilcoxon
#------------------------------------------------------------------------------

# Crear una tabla para almacenar los resultados de los tests
tb_resultados_wilcoxon <- data.table(
  envio = integer(),
  modelo_1 = character(),
  modelo_2 = character(),
  p_valor = numeric()
)

# Comparar cada par de modelos, por cada cantidad de envíos
for (k in 1:length(envios)) {  # Iterar sobre cada cantidad de envíos
  for (i in 1:(length(ganancias_modelos) - 1)) {
    for (j in (i + 1):length(ganancias_modelos)) {
      # Extraer las ganancias de los dos modelos a comparar para el envío 'k'
      modelo_1_ganancias <- unlist(ganancias_modelos[[i]][k,])
      modelo_2_ganancias <- unlist(ganancias_modelos[[j]][k,])
      
      # Realizar el test de Wilcoxon para este envío
      test_wilcoxon <- wilcox.test(modelo_1_ganancias, modelo_2_ganancias, paired = TRUE)
      
      # Guardar el resultado en la tabla
      tb_resultados_wilcoxon <- rbind(tb_resultados_wilcoxon, list(
        envio = envios[k],
        modelo_1 = workflows[i],
        modelo_2 = workflows[j],
        p_valor = test_wilcoxon$p.value
      ))
    }
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
), by = .(modelo_1, modelo_2)]

#------------------------------------------------------------------------------
# 7. Guardar los resultados de los p-valores mínimos (original y ajustado)
#------------------------------------------------------------------------------

# Guardar los p-valores mínimos originales y ajustados, junto con las cantidades de envíos correspondientes, en un archivo .txt
fwrite(tb_pvalores_minimos, 
       file = "tb_pvalores_minimos_por_comparacion.txt", 
       sep = "\t")

# Fin del script