# ==============================================================================
#METRICASS

library(tidyverse)
library(tidytext)

#------------------------ 1. CARGA DE DATOS-------------------------------------
input_path <- "TP2/output/processed_text.rds"
message("Cargando datos procesados desde: ", input_path)
tabla_final <- read_rds(input_path)

# ----------------------2. CONSIGNA A: COMPUTAR LA DTM )------------------------
message("computando la Matriz de Frecuencia de Términos (DTM)...")

# claculo cuantas veces aparece la palabra 
frecuencias_doc <- tabla_final |>
  count(id, lemma, name = "n")

# Aca uso la función cast_dtm()  
# para convertir la tabla en una Matriz de Frecuencia 
dtm_oea <- frecuencias_doc |>
  cast_dtm(document = id, term = lemma, value = n)

#----------------- 3. FILTRAR Y CONDENSAR 5 TÉRMINOS RELEVANTES-----------------
terminos_elegidos <- c("democracia", "derecho", "seguridad", "misión", "elección")

message("calculando frecuencias de los 5 términos seleccionados...")

# agarrro la tabla de freeucncias para filtrar, agrupar y sumar
frecuencias_totales <- frecuencias_doc |>
  filter(lemma %in% terminos_elegidos) |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n)) |>
  arrange(desc(frecuencia_total))

#-----------------------. 4. CONSIGNA B: GRAFICO---------------------
message("Generando gráfico de barras c...")

# aca armo el graf de barras
grafico_frecuencias <- frecuencias_totales |>
  ggplot(aes(x = reorder(lemma, frecuencia_total), y = frecuencia_total, fill = lemma)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + # uso esto para q las barras sean horizontales
  labs(
    title = "Frecuencia de términos clave en comunicados de la OEA",
    subtitle = "Período: Enero - Abril 2026",
    x = "Término (Lematizado)",
    y = "Frecuencia Total",
    caption = "Fuente: Elaboración propia en base a www.oas.org"
  ) +
  theme_minimal()

# ------------------------5. GUARDO EL GRAF EN OUTPUT -----------------
output_dir <- "TP2/output"
archivo_grafico <- paste0(output_dir, "/frecuencia_terminos.png")

message("Guardando la figura...")
# ggsave para guardar
ggsave(filename = archivo_grafico, plot = grafico_frecuencias, width = 8, height = 6, dpi = 300)

message(" grafico guardado en ", archivo_grafico, ":)")