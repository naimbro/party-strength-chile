# Análisis de Fortaleza Electoral Partidaria Municipal - Chile

## Descripción

Este proyecto analiza la fortaleza electoral de los partidos políticos en las elecciones municipales chilenas desde 2004 hasta 2024. Desarrolla un **Índice de Fortaleza Partidaria Municipal** que mide el desempeño electoral de los partidos a través de cuatro indicadores estandarizados.

## Metodología

### Índice de Fortaleza Partidaria Municipal

El índice combina cuatro indicadores complementarios, cada uno normalizado en una escala de 0 a 1:

1. **Presencia de candidatos militantes**: Porcentaje de candidatos afiliados a partidos sobre el total de candidatos
2. **Éxito electoral de militantes**: Porcentaje de militantes electos sobre el total de personas electas  
3. **Cobertura territorial**: Porcentaje de comunas donde los partidos presentan al menos un candidato militante
4. **Continuidad de incumbentes**: Tasa de reelección de autoridades militantes que buscan un nuevo mandato

El índice final es el promedio de los indicadores disponibles (3 indicadores cuando falta información de reelección, 4 cuando está completa).

### Datos

- **Fuente**: Servicio Electoral (SERVEL)
- **Período**: 2004-2024
- **Cobertura**: Todas las comunas de Chile
- **Tipos de elección**: Alcaldes y Concejales

## Estructura del Proyecto

```
├── scripts/
│   ├── 02_party_strength_indicators_comuna.R    # Cálculo de indicadores por comuna
│   └── create_interactive_plots_comuna.R        # Generación de visualizaciones
├── data/
│   ├── party_strength_analysis_data.rds         # Datos procesados originales
│   └── party_strength_comuna_data.rds          # Datos con análisis por comuna
├── outputs/
│   ├── analisis_fortaleza_partidaria_comuna.html           # Página principal de análisis
│   ├── indice_fortaleza_partidaria_comuna_data.xlsx        # Datos para descarga
│   ├── pisi_alcaldes_interactive.html                      # Gráfico interactivo alcaldes
│   ├── pisi_concejales_interactive.html                    # Gráfico interactivo concejales
│   └── [otros archivos de gráficos y datos]
├── CUT_comuna.csv                              # Códigos únicos territoriales
└── tabla_comunas_labmun.xlsx                   # Tabla de comunas (respaldo)
```

## Uso

### Ejecutar el análisis

1. **Cálculo de indicadores**:
```r
source("scripts/02_party_strength_indicators_comuna.R")
```

2. **Generación de visualizaciones**:
```r
source("scripts/create_interactive_plots_comuna.R")
```

### Visualizar resultados

Abrir `outputs/analisis_fortaleza_partidaria_comuna.html` en un navegador web para acceder a:
- Gráficos estáticos de evolución temporal nacional
- Gráficos interactivos por comuna con selector múltiple
- Datos para descarga en formato Excel

## Hallazgos Principales

El análisis revela que los partidos políticos en Chile han experimentado un debilitamiento electoral sostenido a nivel municipal desde 2004. Aunque la presencia territorial se ha mantenido relativamente alta, la efectividad electoral y la capacidad de reelección de incumbentes militantes se han erosionado significativamente.

## Requisitos

### Paquetes R necesarios
```r
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(rio)
library(scales)
library(jsonlite)
library(htmltools)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(readxl)
library(janitor)
```

## Autor

**Naim Bro**  
Universidad Adolfo Ibáñez  
Laboratorio Municipal (LabMun)

## Datos

Análisis de elecciones municipales chilenas basado en datos del Servicio Electoral (SERVEL).

---

*Este análisis forma parte del proyecto de investigación sobre institucionalidad política local en Chile.*