# ==============================================================================
# PROCESAMIENTO DE (NLP)

library(tidyverse)
library(udpipe)
##install.packages("stopwords")
library(stopwords)
library(here)

# ---------------------1. CARGA DE DATOS Y DIRECTORIOS.-----------------------------
input_path <- here("TP2", "data", "tabla_oea.rds")
output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) {
  message("Creando el directorio: ", output_dir)
  dir.create(output_dir, recursive = TRUE)
}

message("Cargando datos de: ", input_path)
tabla_oea <- read_rds(input_path)

# ------------------------------------2. LIMPIEZA INICIAL –------------------------------
message("LIMPIEZA: caracteres especiales, números y puntuación...")
tabla_limpia <- tabla_oea |>
  mutate(cuerpo_limpio = cuerpo |>
           str_replace_all("[[:punct:]]", " ") |> 
           str_replace_all("[[:digit:]]", " ") |> 
           str_replace_all("[\\s]+", " ") |>      
           str_trim())

# ---------------------------------3. LEMATIZACIÓN ---------------------------------------------
message("Iniciando lematización con udpipe...")
# cargo el modelo de udpipe 
m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

# ahora lematizo el tetxo completo 
noticias_lemas <- udpipe_annotate(
  modelo_es, 
  x = tabla_limpia$cuerpo_limpio, 
  doc_id = tabla_limpia$id
) |> 
  as.data.frame() |>
  mutate(id = doc_id) |> 
  select(id, lemma, upos)

# agrego los titularres
noticias_lemas <- noticias_lemas |> 
  left_join(
    tabla_limpia |> select(id, titulo), 
    by = "id"
  )

#----------------------------- 4. SACAR STOPWORDS Y SELECCIÓN DE PALABRAS ---------------------------------
message(" filtrando palabras relevantes y borrando stopwords...")

# uso el paquete de stopwords 
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

# lo convieeerto en tabla 
tabla_final <- as_tibble(noticias_lemas) |>
  # sustantivos, VERBOS y adjetivos
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  # escritos en minúscula
  mutate(lemma = str_to_lower(lemma)) |>
  # borrar stopwords
  anti_join(stop_words, by = "lemma") |>
  # Filtro palabras mayores a 2 caracteres
  filter(str_length(lemma) > 2)

#---------------------------- 5. GUARDAR RESULTADO-----------------------------------
output_path <- paste0(output_dir, "/processed_text.rds")
write_rds(tabla_final, output_path)

message(" FINALIZADOS :) Archivo guardado en: ", output_path)