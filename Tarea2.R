# Cargar librerías necesarias
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(forcats)
install.packages("viridis")
library(viridis)
install.packages("reshape2")
library(reshape2)

# 1. Carga y exploración inicial de datos
df <- read.csv("E:/MEGAsync/Materias/Bootcamp KODIGO/Bootcamp-Kodigo-Tareas/Datasets/Accidentalidad_Municipio_de__Envigado_20250606.csv", stringsAsFactors = FALSE)

# Mostrar primeras filas y estructura
head(df)
str(df)
summary(df)

# 2. Limpieza y transformación de datos

# Convertir la columna de fecha a formato Date o POSIXct
df$FECHA <- parse_date_time(df$FECHA, orders = c("Ymd HMS", "Ymd", "dmy HMS", "dmy", "ymd HMS", "ymd", "Y-m-d H:M:S", "Y-m-d"))

# Extraer componentes de la fecha
df$MES <- month(df$FECHA)
df$AÑO <- year(df$FECHA)
df$DIA_MES <- day(df$FECHA)

# Limpiar la columna de hora y extraer la hora en formato numérico
limpiar_hora <- function(hora_str) {
  if (is.na(hora_str) || hora_str == "") return(NA)
  hora_str <- tolower(trimws(hora_str))
  hora_str <- gsub("a\\. m\\.", "AM", hora_str)
  hora_str <- gsub("p\\. m\\.", "PM", hora_str)
  tryCatch({
    hora <- hour(parse_date_time(hora_str, orders = c("I:M:S p", "H:M:S")))
    return(hora)
  }, error = function(e) { return(NA) })
}
df$HORA_DIA <- sapply(df$HORA, limpiar_hora)

# Agrupar horas en franjas horarias
df$FRANJA_HORARIA <- case_when(
  is.na(df$HORA_DIA) ~ "Sin dato",
  df$HORA_DIA >= 6 & df$HORA_DIA < 12 ~ "Mañana",
  df$HORA_DIA >= 12 & df$HORA_DIA < 18 ~ "Tarde",
  df$HORA_DIA >= 18 & df$HORA_DIA < 24 ~ "Noche",
  TRUE ~ "Madrugada"
)

# Rellenar valores nulos en columnas categóricas
cat_cols <- c("CLASE.DE.VEHICULO", "TIPO.DE.SERVICIO", "TIPO.DE.VICTIMA", "SEXO")
for (col in cat_cols) {
  if (col %in% names(df)) {
    df[[col]][is.na(df[[col]]) | df[[col]] == ""] <- "Sin dato"
  }
}
if ("DIRECCIÓN" %in% names(df)) df$DIRECCIÓN[is.na(df$DIRECCIÓN)] <- "No especificado"

# Extraer coordenadas
extraer_coordenadas <- function(punto) {
  punto <- trimws(as.character(punto))
  punto <- gsub("POINT \\(|\\)", "", punto)
  coords <- strsplit(punto, " ")[[1]]
  if (length(coords) == 2) {
    return(as.numeric(coords))
  } else {
    return(c(0, 0))
  }
}
coords <- t(sapply(df$Coordenadas, extraer_coordenadas))
df$LONGITUD <- coords[,1]
df$LATITUD <- coords[,2]

# 3. Análisis exploratorio y visualizaciones

# Gravedad de los accidentes
print(table(df$GRAVEDAD))
ggplot(df, aes(y = GRAVEDAD)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de la Gravedad de los Accidentes", y = "Gravedad", x = "Cantidad de Accidentes") +
  theme_minimal()

# Clase de accidente
print(table(df$CLASE.DE.ACCIDENTE))
ggplot(df, aes(y = CLASE.DE.ACCIDENTE)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribución de Clases de Accidentes", y = "Clase de Accidente", x = "Cantidad de Accidentes") +
  theme_minimal()

# Top 10 causas de accidentes
top_causas <- df %>%
  count(CAUSA, sort = TRUE) %>%
  top_n(10, n)
print(top_causas)
ggplot(top_causas, aes(x = n, y = fct_reorder(CAUSA, n))) +
  geom_col(fill = "tomato") +
  labs(title = "Top 10 Causas de Accidentes", x = "Cantidad de Accidentes", y = "Causa") +
  theme_minimal()

# Distribución por día de la semana
if ("DÍA.DE.LA.SEMANA" %in% names(df)) {
  ggplot(df, aes(x = DÍA.DE.LA.SEMANA)) +
    geom_bar(fill = "purple") +
    labs(title = "Distribución de Accidentes por Día de la Semana", x = "Día de la Semana", y = "Cantidad de Accidentes") +
    theme_minimal()
}

# Distribución por mes
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
df$MES_NOMBRE <- factor(meses[df$MES], levels = meses)
ggplot(df, aes(x = MES_NOMBRE)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribución de Accidentes por Mes", x = "Mes", y = "Cantidad de Accidentes") +
  theme_minimal()

# Distribución por franja horaria
ggplot(df, aes(x = FRANJA_HORARIA)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribución de Accidentes por Franja Horaria", x = "Franja Horaria", y = "Cantidad de Accidentes") +
  theme_minimal()

# Top 10 barrios con más accidentes
if ("BARRIO" %in% names(df)) {
  top_barrios <- df %>%
    count(BARRIO, sort = TRUE) %>%
    top_n(10, n)
  print(top_barrios)
  ggplot(top_barrios, aes(x = n, y = fct_reorder(BARRIO, n))) +
    geom_col(fill = "darkred") +
    labs(title = "Top 10 Barrios con Mayor Número de Accidentes", x = "Cantidad de Accidentes", y = "Barrio") +
    theme_minimal()
}

# Clase de accidente vs Gravedad (stacked bar)
if ("CLASE.DE.ACCIDENTE" %in% names(df) && "GRAVEDAD" %in% names(df)) {
  ggplot(df, aes(x = CLASE.DE.ACCIDENTE, fill = GRAVEDAD)) +
    geom_bar(position = "stack") +
    labs(title = "Clase de Accidente vs Gravedad", x = "Clase de Accidente", y = "Cantidad de Accidentes") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Mapa de calor: Día de la semana vs Franja horaria
if ("DÍA.DE.LA.SEMANA" %in% names(df)) {
  heat_data <- df %>%
    group_by(DÍA.DE.LA.SEMANA, FRANJA_HORARIA) %>%
    summarise(count = n()) %>%
    ungroup()
  ggplot(heat_data, aes(x = FRANJA_HORARIA, y = DÍA.DE.LA.SEMANA, fill = count)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(title = "Mapa de Calor: Día de la Semana vs Franja Horaria", x = "Franja Horaria", y = "Día de la Semana") +
    theme_minimal()
}

# Gráfico circular para distribución de Área
if ("AREA" %in% names(df)) {
  area_counts <- df %>% count(AREA)
  ggplot(area_counts, aes(x = "", y = n, fill = AREA)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Distribución de Accidentes por Área") +
    theme_void()
}

# Gráfico de violín: Distribución de accidentes por hora y día de la semana
if ("DÍA.DE.LA.SEMANA" %in% names(df)) {
  ggplot(df, aes(x = DÍA.DE.LA.SEMANA, y = HORA_DIA, fill = DÍA.DE.LA.SEMANA)) +
    geom_violin(trim = FALSE) +
    labs(title = "Distribución de Accidentes por Hora y Día de la Semana", x = "Día de la Semana", y = "Hora del Día") +
    theme_minimal()
}

# Conclusiones
cat("
Principales hallazgos del análisis exploratorio:

1. Distribución temporal de accidentes:
   - Se observa una mayor concentración de accidentes en días laborables.
   - Las horas pico muestran mayor incidencia, coincidiendo con horarios de desplazamiento laboral.
   - La franja horaria de tarde concentra la mayor cantidad de accidentes.

2. Tipos y causas de accidentes:
   - Los choques representan la mayoría de los incidentes.
   - Las principales causas identificadas son relacionadas con comportamiento del conductor.
   - La mayoría de los accidentes resultan en daños materiales.

3. Distribución geográfica:
   - Existe una concentración significativa de accidentes en ciertos barrios.
   - La zona urbana presenta significativamente más accidentes que la rural.

4. Recomendaciones:
   - Reforzar la presencia de agentes de tránsito en días y horas de mayor accidentalidad.
   - Campañas educativas enfocadas en las principales causas identificadas.
   - Mejorar infraestructura en los barrios con mayor incidencia.
")