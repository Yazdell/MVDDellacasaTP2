#Cargar librerías necesarias (Tutorial 05)
library(tidyverse)
library(rvest)
library(tidyr)

data_dir <- "TP2/data"

if (!dir.exists(data_dir)) {
  message("Creando el directorio: ", data_dir)
  dir.create(data_dir, recursive = TRUE)
} else {
  message("El directorio ", data_dir, " ya existe.")
}

meses <- 1:4
anio <- 2026

# PARTE 1: RECOPILAR LOS LINKS DE TODOS LOS COMUNICADOS
message("Iniciando la recolección de links...")
links_totales <- c() # Acá vamos a guardar todos los links que encontremos

for (mes in meses) {
  url_mes <- paste0("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=", mes, "&nAnio=", anio)
  
  # Leemos la página del índice del mes
  pagina_mes <- read_html(url_mes)
  
  # Extraemos todos los links (etiqueta 'a', atributo 'href')
  links_mes <- pagina_mes %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # [CÓDIGO EXTERNO ACLARADO]: Usamos str_detect de la librería stringr (incluida en tidyverse)
  # para filtrar únicamente los links que contienen "sCodigo=", que es la forma en 
  # la que la OEA identifica las URLs de sus comunicados individuales.
  links_comunicados <- links_mes %>%
    na.omit() %>%
    str_subset("sCodigo=")
  
  # Armamos la URL completa (ya que suelen ser relativas en la página)
  links_completos <- paste0("https://www.oas.org/es/centro_noticias/", links_comunicados)
  
  # Los sumamos a nuestra lista total
  links_totales <- c(links_totales, links_completos)
  
  Sys.sleep(3) # Respetamos el robots.txt en esta primera pasada
}

# Nos aseguramos de que no haya links repetidos
links_totales <- unique(links_totales)
message("Se encontraron ", length(links_totales), " comunicados en total.")

# PARTE 2: DESCARGAR LOS HTML INDIVIDUALES
message("Iniciando la descarga de los HTML individuales...")
fecha_hoy <- Sys.Date()

for (i in seq_along(links_totales)) {
  url_comunicado <- links_totales[i]
  
  # Extraemos el ID del comunicado de la URL para usarlo en el nombre del archivo
  # Reemplazamos las barras "/" por guiones bajos "_" para que Windows/Mac no tire error de carpeta
  id_comunicado <- str_extract(url_comunicado, "(?<=sCodigo=).*") %>%
    str_replace_all("/", "_")
  
  # Definimos el nombre del archivo cumpliendo la consigna
  archivo_html <- paste0(data_dir, "/comunicado_", id_comunicado, "_obtenido_", fecha_hoy, ".html")
  
  if (!file.exists(archivo_html)) {
    message("Descargando comunicado ", i, " de ", length(links_totales), ": ", id_comunicado)
    download.file(url = url_comunicado, destfile = archivo_html, quiet = TRUE)
    
    Sys.sleep(3) # Respetamos el Crawl-delay de 3 segundos
  }
}

message("¡Proceso de descarga de HTML finalizado!")


# PARTE 3: CONVERTIR A FORMATO TABULAR (Consigna)
message("Procesando los archivos HTML locales para armar la tabla...")

# Usamos list.files para leer todo lo que hay en la carpeta data
archivos_descargados <- list.files(path = data_dir, pattern = "\\.html$", full.names = TRUE)

# Creamos vectores vacíos (estructura clásica de bucles del Tutorial 04)
ids <- c()
titulos <- c()
cuerpos <- c()

for (archivo in archivos_descargados) {
  
  # Leemos el archivo físico (Tutorial 05)
  pagina <- read_html(archivo)
  
  # 1. Extraemos el ID limpiando el texto con stringr (Tutorial 05)
  # Usamos basename() para quedarnos solo con el nombre del archivo (sin la carpeta)
  nombre_archivo <- basename(archivo) 
  
  # Usamos str_remove para borrar las partes que agregamos nosotros
  id_actual <- nombre_archivo %>% 
    str_remove("comunicado_") %>% 
    str_remove("_obtenido_.*") # Borra desde la palabra "obtenido" hasta el final
  
  # 2. Extraemos el Título (Selector h4)
  # Usamos html_node (singular) para que traiga estrictamente el primer h4
  titulo_actual <- pagina %>% 
    html_node("h4") %>% 
    html_text(trim = TRUE)
  
  # 3. Extraemos el Cuerpo (Selector p)
  # Usamos html_nodes (plural) porque hay varios párrafos
  cuerpo_nodos <- pagina %>% 
    html_nodes("p") %>% 
    html_text(trim = TRUE)
  
  # Unimos los párrafos con paste(collapse) como muestra el Tutorial 05
  cuerpo_actual <- paste(cuerpo_nodos, collapse = " ")
  
  # Guardamos la info de este ciclo en nuestros vectores
  ids <- c(ids, id_actual)
  titulos <- c(titulos, titulo_actual)
  cuerpos <- c(cuerpos, cuerpo_actual)
}

# Armamos el dataframe usando tibble (Tutorial 05)
tabla_oea <- tibble(
  id = ids,
  titulo = titulos,
  cuerpo = cuerpos
)

# Guardamos la tabla como .rds en la carpeta data (Consigna)
archivo_rds <- paste0(data_dir, "/tabla_oea.rds")
write_rds(tabla_oea, file = archivo_rds)

message("¡Tabla creada y guardada con éxito!")
