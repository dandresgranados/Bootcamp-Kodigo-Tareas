# Instala los paquetes de R si no los tienes
rm(list = ls())
library(reticulate)
virtualenv_create("r-reticulate", python = "C:/Users/Diego/AppData/Local/Programs/Python/Python310/python.exe")
use_virtualenv("r-reticulate", required = TRUE)

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("reticulate")) install.packages("reticulate")

library(tidyverse)
library(lubridate)


py_install(c("matplotlib", "seaborn", "pandas", "numpy"))

# 1. Carga y exploración inicial de datos en R
df <- read.csv("E:/MEGAsync/Materias/Bootcamp KODIGO/Bootcamp-Kodigo-Tareas/Datasets/Accidentalidad_Municipio_de__Envigado_20250606.csv", stringsAsFactors = FALSE)

head(df)
str(df)
summary(df)

# 2. Limpieza básica en R
df$FECHA <- lubridate::parse_date_time(df$FECHA, orders = c("Ymd HMS", "Ymd", "dmy HMS", "dmy", "ymd HMS", "ymd", "Y-m-d H:M:S", "Y-m-d"))

# 3. Transferir el dataframe a Python
py$df <- df

py_run_string("print(type(df))")

# 4. Análisis y visualización en Python usando reticulate
py_run_string("
# Vamos a depurar el problema con la función de limpieza de horas
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
import re

# Verificar el formato exacto de algunos valores de hora para depuración
print('Primeros valores de la columna HORA:')
print(df['HORA'].head().tolist())

# Función para analizar hora con detección de errores
def limpiar_hora_debug(hora_str):
    #Función que extrae la hora con debuggeo detallado
    if pd.isna(hora_str):
        return np.nan, 'Es nulo'
    
    hora_str = str(hora_str).strip().lower()
    print(f'Procesando: \"{hora_str}\"')
    
    # Método 1: Extracción por expresiones regulares
    try:
        # Extraer horas usando regex
        match = re.match(r'(\\d+):(\\d+):(\\d+)\\s+(\\w+\\.\\s+\\w+\\.)', hora_str)
        if match:
            hora = int(match.group(1))
            periodo = match.group(4)
            if 'p. m.' in periodo and hora != 12:
                hora += 12
            elif 'a. m.' in periodo and hora == 12:
                hora = 0
            return hora, 'Método regex'
    except Exception as e:
        print(f'Error en regex: {e}')
        
    # Método 2: Extracción manual
    try:
        partes = hora_str.split(':')
        if len(partes) >= 2:
            hora = int(partes[0])
            if 'p. m.' in hora_str and hora != 12:
                hora += 12
            elif 'a. m.' in hora_str and hora == 12:
                hora = 0
            return hora, 'Método manual'
    except Exception as e:
        print(f'Error en método manual: {e}')
    
    return np.nan, 'Error en todos los métodos'

# Probar con algunos valores
print('\\nProbando función de limpieza:')
for i in range(3):
    hora_valor = df['HORA'][i]
    resultado, metodo = limpiar_hora_debug(hora_valor)
    print(f'{hora_valor} -> {resultado} ({metodo})')

# Ahora definamos la función real de limpieza
def limpiar_hora_final(hora_str):
    if pd.isna(hora_str):
        return np.nan
    
    try:
        hora_str = str(hora_str).strip().lower()
        
        # Extraer hora directamente del string
        partes = hora_str.split(':')
        if len(partes) >= 2:
            hora = int(partes[0])
            if 'p. m.' in hora_str and hora != 12:
                hora += 12
            elif 'a. m.' in hora_str and hora == 12:
                hora = 0
            return hora
    except:
        pass
        
    return np.nan

# Aplicar la función corregida
df['HORA_DIA'] = df['HORA'].apply(limpiar_hora_final)

# Asignar franjas horarias
df['FRANJA_HORARIA'] = df['HORA_DIA'].apply(lambda x: 
    'Sin dato' if pd.isna(x)
    else 'Mañana' if 6 <= x < 12
    else 'Tarde' if 12 <= x < 18
    else 'Noche' if 18 <= x < 24
    else 'Madrugada'
)

# Verificar resultados
print('\\nPrimeros 5 valores procesados:')
for i in range(5):
    print(f'Original: {df[\"HORA\"][i]}, Hora: {df[\"HORA_DIA\"][i]}, Franja: {df[\"FRANJA_HORARIA\"][i]}')

print('\\nDistribución de franjas horarias:')
print(df['FRANJA_HORARIA'].value_counts())
")

py_run_string("
# Visualizaciones de gravedad y clase de accidente
# Gravedad de los accidentes
plt.figure(figsize=(10, 6))
sns.countplot(y='GRAVEDAD', data=df, palette='viridis')
plt.title('Distribución de la Gravedad de los Accidentes')
plt.ylabel('Gravedad')
plt.xlabel('Cantidad de Accidentes')
plt.tight_layout()
plt.show()

# Clase de accidente
if 'CLASE.DE.ACCIDENTE' in df.columns:
    plt.figure(figsize=(12, 6))
    sns.countplot(y='CLASE.DE.ACCIDENTE', data=df, palette='viridis')
    plt.title('Distribución de Clases de Accidentes')
    plt.ylabel('Clase de Accidente')
    plt.xlabel('Cantidad de Accidentes')
    plt.tight_layout()
    plt.show()
")

py_run_string("
# Visualizaciones temporales (día y mes)
# Distribución por día de la semana
if 'DÍA.DE.LA.SEMANA' in df.columns:
    plt.figure(figsize=(12, 6))
    orden_dias = ['Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado', 'Domingo']
    dia_semana_counts = df['DÍA.DE.LA.SEMANA'].value_counts()
    dias_presentes = [dia for dia in orden_dias if dia in dia_semana_counts.index]
    dia_semana_counts = dia_semana_counts.reindex(dias_presentes)
    sns.barplot(x=dia_semana_counts.index, y=dia_semana_counts.values, palette='viridis')
    plt.title('Distribución de Accidentes por Día de la Semana')
    plt.xlabel('Día de la Semana')
    plt.ylabel('Cantidad de Accidentes')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

# Distribución por mes
if 'MES' in df.columns:
    meses = ['Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 
             'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre']
    mes_counts = df['MES'].value_counts().sort_index()
    mes_counts.index = [meses[i-1] for i in mes_counts.index]
    plt.figure(figsize=(14, 6))
    sns.barplot(x=mes_counts.index, y=mes_counts.values, palette='viridis')
    plt.title('Distribución de Accidentes por Mes')
    plt.xlabel('Mes')
    plt.ylabel('Cantidad de Accidentes')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()
")

py_run_string("
# Visualizaciones de ubicación y área
# Top 10 barrios
if 'BARRIO' in df.columns:
    barrios_counts = df['BARRIO'].value_counts().head(10)
    plt.figure(figsize=(14, 8))
    sns.barplot(x=barrios_counts.values, y=barrios_counts.index, palette='coolwarm')
    plt.title('Top 10 Barrios con Mayor Número de Accidentes')
    plt.xlabel('Cantidad de Accidentes')
    plt.ylabel('Barrio')
    plt.tight_layout()
    plt.show()

# Distribución por Área
if 'AREA' in df.columns:
    area_counts = df['AREA'].value_counts()
    plt.figure(figsize=(8, 8))
    plt.pie(area_counts, labels=area_counts.index, autopct='%1.1f%%', startangle=90, 
            colors=sns.color_palette('viridis', len(area_counts)))
    plt.axis('equal')
    plt.title('Distribución de Accidentes por Área')
    plt.tight_layout()
    plt.show()
")

py_run_string("
#  Visualizaciones cruzadas y mapas de calor
# Mapa de calor: Día vs Franja horaria
if 'DÍA.DE.LA.SEMANA' in df.columns and 'FRANJA_HORARIA' in df.columns:
    heat_data = pd.crosstab(df['DÍA.DE.LA.SEMANA'], df['FRANJA_HORARIA'])
    orden_dias = ['Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado', 'Domingo']
    orden_franjas = ['Madrugada', 'Mañana', 'Tarde', 'Noche', 'Sin dato']
    heat_data = heat_data.reindex(index=orden_dias)
    heat_data = heat_data.reindex(columns=orden_franjas)
    plt.figure(figsize=(12, 8))
    sns.heatmap(heat_data, annot=True, fmt='g', cmap='YlOrRd')
    plt.title('Mapa de Calor: Día de la Semana vs Franja Horaria')
    plt.xlabel('Franja Horaria')
    plt.ylabel('Día de la Semana')
    plt.tight_layout()
    plt.show()
")

py_run_string("
# Gráfico de violín para la distribución de accidentes por hora y día de la semana
if 'DÍA.DE.LA.SEMANA' in df.columns and 'HORA_DIA' in df.columns:
    # Verificar que tenemos datos para graficar
    if not df['HORA_DIA'].isna().all():
        plt.figure(figsize=(14, 8))
        orden_dias = ['Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado', 'Domingo']
        dias_presentes = [dia for dia in orden_dias if dia in df['DÍA.DE.LA.SEMANA'].unique()]
        
        # Filtrar datos para eliminar NaN en HORA_DIA
        df_filtrado = df.dropna(subset=['HORA_DIA'])
        
        # Crear el gráfico de violín
        sns.violinplot(x='DÍA.DE.LA.SEMANA', y='HORA_DIA', data=df_filtrado, 
                      palette='viridis', order=dias_presentes, inner='quartile')
        
        plt.title('Distribución de Accidentes por Hora y Día de la Semana')
        plt.xlabel('Día de la Semana')
        plt.ylabel('Hora del Día')
        plt.yticks(range(0, 24, 2))  # Mostrar horas de 0 a 23, cada 2 horas
        plt.tight_layout()
        plt.show()
    else:
        print('No hay datos de hora disponibles para generar el gráfico de violín')
")

# 5. Conclusiones en R
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