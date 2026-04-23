#----SCRAPING------

#Cargo las librerias que voy a usar 
library(tidyverse)
library(rvest)
library(tidyr)
library(here)

data_dir <- here("TP2", "data")

if (!dir.exists(data_dir)) {
  message("Creando el directorio: ", data_dir)
  dir.create(data_dir, recursive = TRUE)
} else {
  message("El directorio ", data_dir, " ya existe.")
}

meses <- 1:4
anio <- 2026

#--------------------- PARTE 1: RECOPILO  LOS LINKS ------------------------
message("Iniciando la recolección de links...")
links_totales <- c() # aca voy a guardar los links

for (mes in meses) {
  url_mes <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", mes, "&nAnio=", anio)
  
  # índice del mes
  pagina_mes <- read_html(url_mes)
  
  # Extraigo todos los links 
  links_mes <- pagina_mes %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # filtro los codigos
  links_comunicados <- links_mes %>%
    na.omit() %>%
    str_subset("sCodigo=")
  
  # Armo la URL
  links_completos <- paste0("https://www.oas.org/es/centro_noticias/", links_comunicados)
  
  # Lo sumo a la lista total
  links_totales <- c(links_totales, links_completos)
  
  Sys.sleep(3) #  el robots.txt
}

# chequeo que no se repitan los links
links_totales <- unique(links_totales)
message("Se encontraron ", length(links_totales), " comunicados en total.")



#------------------- PARTE 2: DESCARGAR LOS HTML INDIVIDUALES----------------------------------------
message("Iniciando la descarga de los HTML individuales...")
fecha_hoy <- Sys.Date()

for (i in seq_along(links_totales)) {
  url_comunicado <- links_totales[i]
  
  # Extraigo el ID del comunicado 
  # Reemplazamos las barras "/" por guiones bajos "_" xq sino me tira error
  id_comunicado <- str_extract(url_comunicado, "(?<=sCodigo=).*") %>%
    str_replace_all("/", "_")
  
  # le pongo nombre al archivo
  archivo_html <- paste0(data_dir, "/comunicado_", id_comunicado, "_obtenido_", fecha_hoy, ".html")
  
  if (!file.exists(archivo_html)) {
    message("Descargando comunicado ", i, " de ", length(links_totales), ": ", id_comunicado)
    download.file(url = url_comunicado, destfile = archivo_html, quiet = TRUE)
    
    Sys.sleep(3) # delay de 3 segundos
  }
}

message("Descarga finalizada :)")


#------------------------- PARTE 3: FORMATO TABULAR -----------------------------------
message("Procesando los archivos para hacer la tabla...")

# ahora uso list.files para leer todo lo que hay en la carpeta data
archivos_descargados <- list.files(path = data_dir, pattern = "\\.html$", full.names = TRUE)

# creo vectores vacíos
ids <- c()
titulos <- c()
cuerpos <- c()

for (archivo in archivos_descargados) {
  
  # leer el archivo 
  pagina <- read_html(archivo)
  
  # 1. saco el ID limpiando el texto
  # nombre del archivo 
  nombre_archivo <- basename(archivo) 
  
  # borro 
  id_actual <- nombre_archivo %>% 
    str_remove("comunicado_") %>% 
    str_remove("_obtenido_.*") # borro desde la palabra "obtenido" hasta el final
  
  # 2. extraigo el titulo qu en gadget es  h4  
  # pongo html_node (singular) para que traiga estrictamente el primer h4
  titulo_actual <- pagina %>% 
    html_node("h4") %>% 
    html_text(trim = TRUE)
  
  # 3. extraigo el Cuerpo que en gadget me aparece como p 
  # ca pongo  html_nodes (plural) porque hay varios párrafos
  cuerpo_nodos <- pagina %>% 
    html_nodes("p") %>% 
    html_text(trim = TRUE)
  
  # junto los párrafos con paste(collapse) 
  cuerpo_actual <- paste(cuerpo_nodos, collapse = " ")
  
  # guardo 
  ids <- c(ids, id_actual)
  titulos <- c(titulos, titulo_actual)
  cuerpos <- c(cuerpos, cuerpo_actual)
}

# A¿ahora si hago el dataframe usando tibble 
tabla_oea <- tibble(
  id = ids,
  titulo = titulos,
  cuerpo = cuerpos
)

# guardo la tabla como .rds en la carpeta data 
archivo_rds <- paste0(data_dir, "/tabla_oea.rds")
write_rds(tabla_oea, file = archivo_rds)

message("¡Tabla creada y guardada con éxito!")
